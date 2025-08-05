#lang sicp


(define (make-account balance password)
  (let ((miss-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops amount) "call the cops")
    (define (incorrect-password amount)
      (begin
        (set! miss-count (+ miss-count 1))
        "Incorrect password"))
    (define (reset-miss-count)
      (set! miss-count 0))
    
    (define (dispatch p m)
      (cond
        ((> miss-count 6) call-the-cops) ; 7回ミスってたらなにもできなくなる
        ((not (eq? p password)) incorrect-password) ; ミスった時の処理
        ((eq? p password) ;通常処理
         (begin
           (reset-miss-count) ; 7連続ミスっていう仕様なので、成功したら、countをリセットする
           (cond
             ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             (else (error "Unknown request -- MAKE-ACCOUNT"
                          m)))))))
    dispatch))


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
; "call the cops"
((acc 'secret-password 'withdraw) 40)
; "call the cops"

