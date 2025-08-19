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
  (define (incorrect-password amount)
    "Incorrect password")
  (define (dispatch p m)
    (cond
      ((not (eq? p password)) incorrect-password)
      ((eq? m 'check-password) #t) ; 追加した。ここにきているってことはtrueになるので、常に#tをかえす
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request -- MAKE-ACCOUNT"
                   m))))
  dispatch)


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
; 60
((acc 'some-other-password 'deposit) 50)
; Incorrect password

(define (make-joint account acc-password my-password)
  (if (account acc-password 'check-password)
      (lambda (p m)
        (if (eq? p my-password)
            (account acc-password m)  ; 元の口座を呼び出す
            (lambda (amount) "Incorrect password")))
      (error "Incorrect password for joint account")))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 40)  ; 元のパスワードで引き出し
; 60
((paul-acc 'rosebud 'withdraw) 20)       ; 新しいパスワードで同じ口座から引き出し
; 40
((paul-acc 'ros 'withdraw) 20)       ; 新しいパスワードで同じ口座から引き出し
; Incorrect password
((paul-acc 'rosebud 'withdraw) 20)       ; 新しいパスワードで同じ口座から引き出し
; 20