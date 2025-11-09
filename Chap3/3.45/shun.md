デッドロックが起きてしまうため正しくない。

withdrawを`(balance-serializer withdraw)`、depositを`(balance-serializer deposit)`とする。

```racket
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
```

この時、serialized-exchangeはその手続きの中でアカウント1のロックとアカウント2のロックを取る。

exchange手続きの中では`(account1 'withdraw)`が呼ばれ、これもまた、`(balance-serializer withdraw)`でロックをとりに行こうとする。

しかし、アカウント1についてはserialized-exchangeの時点でロックをとってしまっているため、ロックが開かないというデッドロックの状態になってしまい処理が止まってしまう。