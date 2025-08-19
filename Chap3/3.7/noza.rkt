#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'check-password) #t)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (if (eq? m 'check-password)
            #f
            (lambda (x) "Incorrect password"))))
  dispatch)

(define (make-joint account account-password new-password)
  (if (account account-password 'check-password)
      (lambda (pwd m)
        (if (eq? pwd new-password)
            (account account-password m)
            (lambda (x) "Incorrect password")))
      (error "Incorrect password for original account")))

;; テスト
(define peter-acc (make-account 100 'open-sesame))

(display "Peter's initial balance after withdrawing 10: ")
(display ((peter-acc 'open-sesame 'withdraw) 10))
(newline)

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

(display "Paul withdraws 20 with his password: ")
(display ((paul-acc 'rosebud 'withdraw) 20))
(newline)

(display "Peter checks balance after Paul's withdrawal: ")
(display ((peter-acc 'open-sesame 'withdraw) 0))
(newline)

(display "Paul deposits 30: ")
(display ((paul-acc 'rosebud 'deposit) 30))
(newline)

(display "Peter's balance after Paul's deposit: ")
(display ((peter-acc 'open-sesame 'withdraw) 0))
(newline)

(display "Wrong password for Paul: ")
(display ((paul-acc 'wrong-password 'withdraw) 10))
(newline)
