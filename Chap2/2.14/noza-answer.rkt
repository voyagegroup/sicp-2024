#lang racket

(define (make-interval a b)
    (cons a b))

(define (lower-bound i)
    (car i))

(define (upper-bound i)
    (cdr i))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c t)
    (make-interval (- c (* c (/ t 100))) (+ c (* c (/ t 100)))))

(define (percent i)
    (* (/ (width i) (center i) ) 100))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

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

(par1 (make-center-percent 10 10) (make-center-percent 20 5))
(par2 (make-center-percent 10 10) (make-center-percent 20 5))

; 実際に異なる

(par1 (make-center-percent 10 0.01) (make-center-percent 20 0.02))
(par2 (make-center-percent 10 0.01) (make-center-percent 20 0.02))

; 誤差が小さいとは計算結果が少なくなる

; 感覚的に考えると、掛け算の時に発生する誤差の積の影響が大きくなりそう
; part2 に比べ part1 の方がトータルの積の回数が多いため、誤差が大きくなりやすい
; ちなみに代数的に展開していくと誤差含めて同じ結果になる
