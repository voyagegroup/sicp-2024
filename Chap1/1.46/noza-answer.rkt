#lang racket



(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (x) (iter x)))

(define (my-sqrt x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (* guess guess) x)) 0.001))
    (lambda (guess)
      (define (average x y)
        (/ (+ x y) 2))
     (average guess (/ x guess))))
   x))

(my-sqrt 9.0)
(my-sqrt 100.0)

(define (fixed-point f first-gess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-gess))

(fixed-point cos 1.0)
