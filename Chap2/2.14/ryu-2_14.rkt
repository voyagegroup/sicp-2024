#lang sicp

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

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; -- 1.12 の手続き --
; 中央値をだす
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

; 幅をだす
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (width i) (center i)))

; -- 必要な手続きはここまで --

(define r1 (make-interval 5 10))
(define r2 (make-interval 15 20))

(par1 r1 r2)
; -> (2.5 . 10.0)
(par2 r1 r2)
; -> (3.75 . 6.66666666666666)

; memo: 手計算したら、par1とpar2ともに(3.75 . 6.66...) になったのでpar1に誤差がでるっぽい


(define r3 (make-interval 5 5.2))
(define r4 (make-interval 15 15.2))

(par1 r3 r4)
; -> (3.6764705882352944 . 3.952)
(par2 r3 r4)
; -> (3.75 . 3.874509803921569)

; memo: 確かに幅が中央値に比べて小さいパーセントだと誤差が小さくなる

; A/A と A/B を計算してみる
(define aa(div-interval r1 r1))
; -> (0.5 . 2.0)
; memo: A/A は 1 にならない

(define ab (div-interval r1 r2))
; -> (0.25 . 0.6666666666666666)


(center aa)
; -> 1.25
(center ab)
; -> 0.4583333333333333

(width aa)
; -> 0.75
(width ab)
; -> 0.20833333333333331

(percent aa)
; -> 0.6
(percent ab)
; -> 0.45454545454545453





















