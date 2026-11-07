```
Welcome to DrRacket, version 8.12 [cs].
Language: sicp, with debugging; memory limit: 128 MB.


;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))


(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 1)

(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 48 maximum-depth = 13)
;;; EC-Eval value:
2

;;; EC-Eval input:

(factorial 3)

(total-pushes = 80 maximum-depth = 18)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 112 maximum-depth = 23)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 144 maximum-depth = 28)
;;; EC-Eval value:
120

;;; EC-Eval input:
.
```

## 問題5.26：反復的階乗計算

| n | total-pushes | maximum-depth |
|---|---:|---:|
| 1 | 64 | 10 |
| 2 | 99 | 10 |
| 3 | 134 | 10 |
| 4 | 169 | 10 |
| 5 | 204 | 10 |

## 問題5.27：再帰的階乗計算

| n | total-pushes | maximum-depth |
|---|---:|---:|
| 1 | 16 | 8 |
| 2 | 48 | 13 |
| 3 | 80 | 18 |
| 4 | 112 | 23 |
| 5 | 144 | 28 |

## 比較

| 項目 | 5.26（反復的） | 5.27（再帰的） |
|---|---|---|
| total-pushes | 35n + 29 | 32n - 16 |
| maximum-depth | 10 | 5n + 3 |
| 空間計算量 | O(1) | O(n) |

