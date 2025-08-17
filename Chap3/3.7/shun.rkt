#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password-error amount)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        incorrect-password-error))
  dispatch)


(define (make-joint acc joined-password new-password)
  (define (incorrect-password-error amount)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p new-password)
        (cond ((eq? m 'withdraw) (lambda (n) ((acc joined-password 'withdraw) n)))
          ((eq? m 'deposit)  (lambda (n) ((acc joined-password 'deposit) n)))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        incorrect-password-error))
  (let ((pre-depo ((acc joined-password 'deposit) 0)))
    (if (and (string? pre-depo) (string=? pre-depo "Incorrect password"))
        (error "incorrect password")
        dispatch)))

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesae 'rosebud))


((paul-acc 'rosebud 'deposit) 10)
; 110

((paul-acc 'rosebud 'withdraw) 30)
; 80

((peter-acc 'open-sesame 'deposit) 0)
; 80