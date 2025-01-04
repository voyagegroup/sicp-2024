#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment s e)
  (cons s e))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2) (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))


(define a
  (make-point 2 3))

(define b
  (make-point 4 1))

(define s
  (make-segment a b))

(print-point (midpoint-segment s))