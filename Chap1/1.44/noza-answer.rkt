#lang racket

(define (dx) 0.00001)

; 前の問題から持ってきたやつ
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated-iter (- n 1)))))
  (repeated-iter n))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x (dx))) (f x) (f (+ x (dx)))) 3)))


(define (n-smooth f n)
  ((repeated smooth n) f))

