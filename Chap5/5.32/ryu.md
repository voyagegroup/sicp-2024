修正前

```
;; 1. 演算子が記号 → 新しい経路
;;; EC-Eval input:
(define (f x y)
  (list x y))

(f 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 16 maximum-depth = 5)
;;; EC-Eval value:
(x y)


;; 2. 演算子が式 → 外側は従来の経路

;;; EC-Eval input:
(define (get-f)
  f)

((get-f) 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 19 maximum-depth = 6)
;;; EC-Eval value:
(x y)


;; 3. 被演算子の中で手続きを呼ぶ

;;; EC-Eval input:
(define (g x)
  (list 'g x))

(define y 'Y)

(f (g 'x) y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 29 maximum-depth = 10)
;;; EC-Eval value:
((g x) Y)

;;; EC-Eval input:
(f (g 'x) 'y)


(total-pushes = 29 maximum-depth = 10)
;;; EC-Eval value:
((g x) y)


;; 4. 引数なしの呼び出し
;;; EC-Eval input:
(define (zero)
  42)

(zero)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
42

;; 5. 呼び出し後に計算を続ける

;;; EC-Eval input:
(cons (zero) '())


(total-pushes = 11 maximum-depth = 8)
;;; EC-Eval value:
(42)
```

修正後

```
;; 1. 演算子が記号 → 新しい経路


;;; EC-Eval input:
(define (f x y)
  (list x y))

(f 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 12 maximum-depth = 5)
;;; EC-Eval value:
(x y)


;; 2. 演算子が式 → 外側は従来の経路
;;; EC-Eval input:
(define (get-f)
  f)

((get-f) 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 15 maximum-depth = 5)
;;; EC-Eval value:
(x y)


;; 3. 被演算子の中で手続きを呼ぶ
;;; EC-Eval input:
(define (g x)
  (list 'g x))

(define y 'Y)

(f (g 'x) y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 21 maximum-depth = 10)
;;; EC-Eval value:
((g x) Y)

;;; EC-Eval input:
(f (g 'x) 'y)


(total-pushes = 21 maximum-depth = 10)
;;; EC-Eval value:
((g x) y)


;; 4. 引数なしの呼び出し
;;; EC-Eval input:
(define (zero)
  42)

(zero)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 1 maximum-depth = 1)
;;; EC-Eval value:
42


;; 5. 呼び出し後に計算を続ける
;;; EC-Eval input:
(cons (zero) '())


(total-pushes = 7 maximum-depth = 6)
;;; EC-Eval value:
(42)

```
