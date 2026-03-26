# 問題 4.25 解答

対象の定義:

```scheme
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))
```

## 結論

- 通常の作用的順序の Scheme では、`(factorial 5)` は停止しない。
- 正規順序の言語では、この定義は動作し、`120` を返す。

## 作用的順序で何が起きるか

`unless` は特殊形式ではなく普通の手続きなので、作用的順序では**手続きを呼ぶ前に引数をすべて評価する**。

したがって

```scheme
(factorial 5)
```

はまず

```scheme
(unless (= 5 1)
        (* 5 (factorial 4))
        1)
```

の形になり、`unless` を適用する前に次を評価しようとする。

- `(= 5 1)` → `#f`
- `(* 5 (factorial 4))`
- `1`

ここで `(* 5 (factorial 4))` を評価するために `(factorial 4)` が必要になる。同様にして

```scheme
(factorial 4)
-> (unless (= 4 1)
           (* 4 (factorial 3))
           1)
```

```scheme
(factorial 3)
-> (unless (= 3 1)
           (* 3 (factorial 2))
           1)
```

と続く。

`(factorial 1)` に達すると

```scheme
(unless (= 1 1)
        (* 1 (factorial 0))
        1)
```

になるが、ここでも作用的順序では `unless` の引数を全部先に評価するため、`condition` が真であっても

```scheme
(* 1 (factorial 0))
```

を評価しようとしてしまう。するとさらに `(factorial 0)`, `(factorial -1)`, ... と進み、再帰が止まらない。

したがって通常の Scheme では `(factorial 5)` は**無限再帰**になり、実際には処理系の再帰限界やスタック制限に達してエラーになる。

## 正規順序ならどうか

正規順序では、引数は必要になるまで評価されない。したがって

```scheme
(factorial 1)
```

は

```scheme
(unless (= 1 1)
        (* 1 (factorial 0))
        1)
```

となっても、まず `unless` の本体

```scheme
(if condition exceptional-value usual-value)
```

の判定に必要な `condition` だけが評価される。

```scheme
(= 1 1) -> #t
```

なので `if` は `exceptional-value` の `1` を返し、`usual-value` である

```scheme
(* 1 (factorial 0))
```

は評価されない。よって再帰は `n = 1` で停止する。

そのため正規順序の言語ではこの `factorial` 定義は正しく動作し、

```scheme
(factorial 5) => 120
```

となる。
