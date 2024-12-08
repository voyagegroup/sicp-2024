#lang racket

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square n)
  (* n n))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

(define (even? n)
  (= (remainder n 2) 0))

(define (expt x n)
  (fast-expt-iter x n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (k) 1)

(define (n-root n x)
  (fixed-point ((repeated average-damp (+ (quotient n 4) 1)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

; k=1 n=1,2,3 
; k=2 n=4,5,6,7
; k=3 n=8~15
; k=4 n=16...

; kは4で割った整数値に1を足したものならよさそう。