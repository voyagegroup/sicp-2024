fibを実行してみる

```
Welcome to DrRacket, version 8.12 [cs].
Language: sicp, with debugging; memory limit: 128 MB.


;;; EC-Eval input:
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
0

;;; EC-Eval input:

(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 72 maximum-depth = 13)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 128 maximum-depth = 18)
;;; EC-Eval value:
2

;;; EC-Eval input:

(total-pushes = 240 maximum-depth = 23)
;;; EC-Eval value:
3

;;; EC-Eval input:

(total-pushes = 408 maximum-depth = 28)
;;; EC-Eval value:
5

;;; EC-Eval input:

(total-pushes = 688 maximum-depth = 33)
;;; EC-Eval value:
8

;;; EC-Eval input:
.
```

実験のまとめ


| n | Fib(n) | total-pushes | maximum-depth |
|---:|---:|---:|---:|
| 0 | 0 | 16 | 8 |
| 1 | 1 | 16 | 8 |
| 2 | 1 | 72 | 13 |
| 3 | 2 | 128 | 18 |
| 4 | 3 | 240 | 23 |
| 5 | 5 | 408 | 28 |
| 6 | 8 | 688 | 33 |


a. n ≥ 2なるFib(n)を計算するのに必要なスタックの最大深さの, n を使った式を与えよ. ヒント: 1.2.2節ではこのプロセスの使うスペースはn に線形に成長することを論じた.

depth = 5n + 3


b. n ≥ 2なるFib(n)を計算するのに使うプッシュの総数の式を与えよ. プッシュの数(これは使う時間とよく相関する.)はnに指数的に成長することを見つけなければならない. ヒント: S(n)をFib(n)を計算するのに使うプッシュの数とする. S(n-1), S(n-2)とnに無関係の一定の「オーバーヘッド」定数k を使ったS(n)を表す式があることを論じなければならない. その式を書き, kが何かを述べよ. 次にS(n)はa Fib(n+1) + bで表せることを示し, aとbの値を与えよ.
