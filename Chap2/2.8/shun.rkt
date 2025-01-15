#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define a (make-interval 5 7))
(define b (make-interval 3 4))

(sub-interval a b)
; Output: (2 . 3)