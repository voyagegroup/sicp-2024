lispの順番や言葉にコンバートしてしまえば良い。
例えば、

```
function (params) body
```

は、以下のように書き換えることで実行することができる

```scheme
(define (lambda? exp)
  (tagged-list? exp 'fun))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'fun (cons parameters body)))
```

