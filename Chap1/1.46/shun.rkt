#lang racket

(define (iterative-improve good? improve)
  (lambda (x)
    (define (iter x)
      (if (good? x)
          x
          (iter (improve x))))
    (iter x)))

(define (sqrt x)
  (define (average x y)
  (/ (+ x y) 2))
  (define (square n)
  (* n n))
  (define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
  (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))

(sqrt 9.0)
; 3.00009155413138


(define (fixed-point f first-guess)
  (define (close-enough? v1)
    (< (abs (- v1 (f v1))) 0.00001))
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)
; 0.7390893414033927