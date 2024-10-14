;
#lang sicp

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (square x)
  (* x x))
(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
     (abs (* 0.001 guess))))

; good-enough

(my-sqrt 1000000000)
; 31638.660565937265
(my-sqrt 0.000000001)
; 0.010000714038711746

