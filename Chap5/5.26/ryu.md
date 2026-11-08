```
Welcome to DrRacket, version 8.12 [cs].
Language: sicp, with debugging; memory limit: 128 MB.


;;; EC-Eval input:
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(factorial 1)

(total-pushes = 64 maximum-depth = 10)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2
)
(total-pushes = 99 maximum-depth = 10)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 134 maximum-depth = 10)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 169 maximum-depth = 10)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5
)
(total-pushes = 204 maximum-depth = 10)
;;; EC-Eval value:
120

;;; EC-Eval input:
.
```


# a. n!の評価に必要な最大深さはnに無関係であることが分るであろう. その深さは何か.

常に10


# b. データからn ≥ 1なるn!の評価に使ったプッシュ演算の総数のnに関する式を決めよ. 使った演算の数はnの線形関数で, 二つの定数で決ることに注意せよ.

(35 * n) + 29


