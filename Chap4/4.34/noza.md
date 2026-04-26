# 問題 4.34 実装方針

## センチネルパラメータ名による遅延対の識別

`cons` の返すクロージャのパラメータ名を `lazy-pair` という目印にする。
メタレベルから `procedure-parameters` を見るだけで遅延対を識別できる。

```scheme
(define (cons x y)
  (lambda (lazy-pair) (lazy-pair x y)))
```

## lazy-pair? の判定

```scheme
(define (lazy-pair? obj)
  (and (compound-procedure? obj)
       (equal? (procedure-parameters obj) '(lazy-pair))))
```

## car/cdr の取り出し

クロージャの環境に `x`/`y` がサンクとして束縛されているので、
`lookup-variable-value` で取り出し `force-it` で強制する。

```scheme
(define (lazy-pair-car obj)
  (force-it (lookup-variable-value 'x (procedure-environment obj))))

(define (lazy-pair-cdr obj)
  (force-it (lookup-variable-value 'y (procedure-environment obj))))
```

## 無限リストの印字

`user-print-lazy-pair` に `limit` カウンタを持たせ、10要素を超えたら `...` で打ち切る。
