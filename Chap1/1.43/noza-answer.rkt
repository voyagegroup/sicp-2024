#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated-iter (- n 1)))))
  (repeated-iter n))

((repeated (lambda (x) (+ x 1)) 8) 5)

((repeated (lambda (x) (* x x)) 3) 3)
