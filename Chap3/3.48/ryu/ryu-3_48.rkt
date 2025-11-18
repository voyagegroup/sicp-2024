#lang sicp

; idを作成する君
(define generate-id
  (let ((id 0))
    (lambda ()
      (set! id (+ id 1))
      id)))

(define (make-account balance)
  (let ((id (generate-id)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (let ((protected (make-serializer)))
      (let ((protected-withdraw (protected withdraw))
            (protected-deposit (protected deposit)))
        (define (dispatch m)
          (cond ((eq? m 'withdraw) protected-withdraw)
                ((eq? m 'deposit) protected-deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'serializer) balance-serializer)
                ((eq? m 'id) id) ;idをかえすようにする
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        dispatch))))


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id)) ; id1のほうがちいさい場合: 1 → 2
    ((serializer1 (serializer2 exchange))
     account1
     account2)
    ((serializer2 (serializer1 exchange)) ; id2のほうが小さい場合: 2 → 1
     account2
     account1)
    )))