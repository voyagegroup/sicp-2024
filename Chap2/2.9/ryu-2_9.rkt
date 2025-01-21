#lang sicp

; 区間の幅(width)は上限と下限の差の半分である. 幅は区間で規定した数の不確かさの量である.
; 算術演算のあるものには, 二つの区間から作った結果の幅は, 引数の幅だけの関数であり, 他のものには, 結果の幅は引数の区間の幅の関数にはならない.
; 二つの区間の和(または差)の幅は, 足されるべき(または引かれるべき)区間の幅だけの関数であることを示せ.
; 乗算と除算についてはこれが成り立たないことを例で示せ.


(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

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



(define interval1 (make-interval 1 3))
(define interval2 (make-interval 4 5))

(div-interval interval1 interval2) ;-> (0.2 . 0.75)
