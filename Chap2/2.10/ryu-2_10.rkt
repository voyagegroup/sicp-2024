#lang sicp

; 経験あるシステムプログラマ, Ben BitdiddleはAlyssaの肩越しに眺め, 零を跨る区間で割った時, どうなるかよく分らないと評した.
; この状態が生じたことを調べ, 起きたらエラーとするようにAlyssaのプログラムを修正せよ.
(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define interval1 (make-interval 5 6))
(define interval2 (make-interval -1 2))
(define interval3 (make-interval 1 2))

; alyssaの例
(define (alyssa-div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; 作ったやつ
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "ゼロをまたがる区間でわろうとしています")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(div-interval interval1 interval3)
; -> (2.5 . 6.0)
(div-interval interval1 interval2)
; -> ゼロをまたがる区間でわろうとしています

