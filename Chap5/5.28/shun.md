## 反復的階乗

depth = 3n + 14

```
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

(total-pushes = 70 maximum-depth = 17)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 107 maximum-depth = 20)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 144 maximum-depth = 23)
;;; EC-Eval value:
6
```


## 再帰的階乗

depth = 8n + 3

```
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

(total-pushes = 18 maximum-depth = 11)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 52 maximum-depth = 19)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 86 maximum-depth = 27)
;;; EC-Eval value:
6
```