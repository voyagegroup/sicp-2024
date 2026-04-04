# 問題 4.29 解答

## 結論

メモ化しない場合、**同じ遅延引数を手続き本体で何度も参照するプログラム**は、同じ計算を何度もやり直すので著しく遅くなる。

例えば次のプログラムはその典型である。

```scheme
(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define (fib n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fib (- n 1))
             (fib (- n 2))))))

(define (use8 x)
  (+ x x x x x x x x))

(use8 (id (fib 24)))
count
```

`use8` の本体では引数 `x` を 8 回使う。
メモ化ありなら `(id (fib 24))` は最初の 1 回だけ評価され、その結果が再利用される。
メモ化なしなら `x` を参照するたびに `(id (fib 24))` が再評価されるので、`fib 24` を 8 回計算することになる。

## `(square (id 10))` の応答

```scheme
(define (square x)
  (* x x))
```

に対して、

```scheme
(square (id 10))
```

の値はどちらの場合も `100` である。

### メモ化する場合

```scheme
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100

;;; L-Eval input:
count
;;; L-Eval value:
1
```

`x` は `(* x x)` の中で 2 回使われるが、最初の強制で `(id 10)` の結果をサンクに記録するので、`id` は 1 回しか実行されない。

### メモ化しない場合

```scheme
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100

;;; L-Eval input:
count
;;; L-Eval value:
2
```

`x` を使うたびに `(id 10)` を再評価するので、`id` が 2 回実行される。
