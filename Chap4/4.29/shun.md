重い処理を行う合成手続を引数に持ち、合成手続きの中でそれを複数呼ぶ場合にはるかに遅くなる。

```scheme
(define (square x)
  (* x x))

(square (fib 100) (fib 100))
```

このとき、メモ化をしない場合は`(fib 100)`が2度計算されてしまう。
しかし、メモ化した場合はforce-itの中でevaluated-thunkに置き換えられて計算結果を使い回すため`(fib 100)`のような重い処理の実行が一度で済む。


```scheme
(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)


(define (square x)
  (* x x))

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
⟨応答⟩

;;; L-Eval input:
count
;;; L-Eval value:
⟨応答⟩
```


### メモ化していない場合

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


### メモ化した場合

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


### 考察

id手続自体はxをそのまま返す手続であり、今回は10を返すと言う挙動は変わらない。
よってsquareの実行結果は両方とも100である。
ただし、メモ化をしない場合はid手続が2回評価されたため、countが2になっている。
メモ化した場合はresultを返すため2回目の評価は行われず、countは1になる。