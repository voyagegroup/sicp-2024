;

#lang sicp

(define (cubic-iter guess x)
  (if (good-enough? guess x)
    guess
    (cubic-iter (improve guess x)
               x)))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))
(define (cube x)
  (* x x x))
(define (cubic x)
  (cubic-iter 1.0 x))

(cubic 8)
; 2.000004911675504
(cubic 27)
; 3.0000005410641766
(cubic 64)
; 4.000017449510739
(cubic 125)
; 5.000000000287929

