## a

depth = 5n + 3

```
(fib 2)

(total-pushes = 72 maximum-depth = 13)
;;; EC-Eval value:
1

;;; EC-Eval input:
(fib 3)

(total-pushes = 128 maximum-depth = 18)
;;; EC-Eval value:
2

;;; EC-Eval input:
(fib 4)

(total-pushes = 240 maximum-depth = 23)
;;; EC-Eval value:
3
```

## b

```
(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
1
```

S(n) は `S(n - 1) + S(n - 2) + k`である。

S(2)は`S(2) = 72 = S(0) + S(1) = 16 + 16 + k`より,`k = 40`である。

`(n - 1), (n - 2)`にはそれぞれ`n < 2`のときに起きるpushの16がオーバーヘッドとしてかかる。

ここで、`T(n) = S(n) + 40`を定義する。

T(n) = S(n) + 40
     = S(n - 1) + 40 + S(n - 2) + 40
     = T(n - 1) + T(n - 2)

つまりT(n)はフィボナッチ数列と同じ漸化式に従う。

S(0) = S(1) = 16であるから

T(0) = T(1) = 56

フィボナッチ数列は

Fib(1) = Fib(2) = 1


従って
T(0) = 56 * Fib(1)
T(1) = 56 * Fib(2)

T(n)と56Fib(n+1)は初期値が一致し、同じ漸化式に従うので、全てのn>=0について
T(n) = 56Fib(n + 1)
が成立する。

T(n) = S(n) + 40
S(n) = T(n) - 40
S(n) = 56Fib(n + 1) - 40
a = 56, b = -40


Fib(n + 1)を使って求められることを示せたならただ連立方程式を使っても求められる

S(n) = a * fib(n+1) + b
128 = a * 3 + b
72 = a * 2 + b

56 = a
b = -40

