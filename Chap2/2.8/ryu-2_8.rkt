#lang sicp
; Alyssaと似たような推論をして, 二つの区間の差の計算法を書け. それに対応する sub-intervalという減算手続きを定義せよ.

; interval
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(define interval1 (make-interval 6.12 7.48)) ; x=6.8 誤差10%
(define interval2 (make-interval 4.465 4.935)) ; y=4.7 誤差5%


(define sub (sub-interval interval1 interval2))
(upper-bound sub) ;-> 3.015
(lower-bound sub) ;-> 1.185

