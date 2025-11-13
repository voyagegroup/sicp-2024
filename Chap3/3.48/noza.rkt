#lang sicp

;; デッドロックが起こらない理由:
;;   任意の2口座 (id1, id2) に対して、常に小さい id 側のシリアライザを
;;   先に獲得し、大きい id 側を後に獲得する。すべてのプロセスが同じ順序
;;   (昇順) でロックを獲得するため、循環待ちの条件が成立せずデッドロックが
;;   発生しない。

(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (acquire)
      (if (test-and-set! cell)
          (acquire)
          'ok))
    (define (release)
      (clear! cell)
      'ok)
    (lambda (message)
      (cond ((eq? message 'acquire) (acquire))
            ((eq? message 'release) (release))
            (else (error "Unknown request -- MUTEX" message))))))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (proc)
      (define (serialized-proc . args)
        (mutex 'acquire)
        (let ((val (apply proc args)))
          (mutex 'release)
          val))
      serialized-proc)))

;; maike-account で id を引数にとる
;; メッセージパッシングで id にアクセスできるように
(define (make-account-and-serializer id balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((serializer (make-serializer)))
    (define (dispatch message)
      (cond ((eq? message 'withdraw) withdraw)
            ((eq? message 'deposit) deposit)
            ((eq? message 'balance) balance)
            ((eq? message 'serializer) serializer)
            ((eq? message 'id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT" message))))
    dispatch))

;; 交換処理
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; id でロックを取る順番を調整する
;; 交換処理はこれまでの実装を利用
(define (serialized-exchange account1 account2)
  (let* ((serializer1 (account1 'serializer))
         (serializer2 (account2 'serializer))
         (id1 (account1 'id))
         (id2 (account2 'id))
         (ordered (if (< id1 id2)
                      (serializer1 (serializer2 exchange))
                      (serializer2 (serializer1 exchange)))))
    (ordered account1 account2)))
