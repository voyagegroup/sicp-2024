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
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) "Incorrect password")))
  dispatch)

;; テスト用のコード
(define acc (make-account 100 'secret-password))

;; 正しいパスワードでwithdraw
(display "正しいパスワードでwithdraw: ")
(display ((acc 'secret-password 'withdraw) 40))
(newline)

;; 間違ったパスワードでdeposit  
(display "間違ったパスワードでdeposit: ")
(display ((acc 'some-other-password 'deposit) 50))
(newline)