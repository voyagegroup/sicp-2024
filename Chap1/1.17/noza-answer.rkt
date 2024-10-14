#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (mul a b)
  (display "mul")
  (newline)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))

(define (double x)
  (+ x x))

(define (helve x) (/ x 2))

(define (fast-multi a b)
  (display "fast-multi")
  (newline)
  (cond 
    ((= b 0) 0)
    ((even? b ) (fast-multi (double a) (helve b)))
    (else (+ a (fast-multi a (- b 1))))))

(mul 2 20)
(fast-multi 2 20)

(display "--------------------")
(newline)
(fast-multi 2 200)
