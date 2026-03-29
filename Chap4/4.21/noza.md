# 問題 4.21 解答

## a. 階乗式の確認と Fibonacci 版

問題文の式:

```scheme
((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
```

この式は `3628800` を返し、`10!` を計算できる。

同じパターン（自分自身を引数で受け取る手続き）で Fibonacci を書くと、例えば次のようになる。

```scheme
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (if (< k 2)
          k
          (+ (fb fb (- k 1))
             (fb fb (- k 2)))))))
 10)
```

この式は `55` を返す。

## b. 空欄補完

空欄を埋めると次の定義になる。

```scheme
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
```

`(f 5)` は `false`、`(f 6)` は `true` になり、内部定義や `letrec` なしで偶奇判定ができる。
