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
   (define (confirm-password p)
    (eq? p password))
  (define (dispatch p m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (define (call-the-cops amount)
    "omawari-san koitsu-desu")
  (let ((incorrect-password-count 0))
    (lambda (p m)
      (if (confirm-password p)
          (begin (set! incorrect-password-count 0) (dispatch p m))
          (begin (set! incorrect-password-count (+ incorrect-password-count 1)) (if (= incorrect-password-count 7) call-the-cops incorrect-password-error))))))


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
; 60

((acc 'some-other-password 'deposit) 50)
; "Incorrect password"
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
; "omawari-san koitsu-desu"