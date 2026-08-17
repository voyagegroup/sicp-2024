#lang sicp

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b) 2))

(define (abs x)
  (if (>= x 0)
      x
      (* x -1)))


(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess) 
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


(sqrt 9)
; 3.00009155413138

