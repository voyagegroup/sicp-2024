#lang racket

(define (make-interval a b)
    (cons a b))

(define (lower-bound i)
    (car i))

(define (upper-bound i)
    (cdr i))

(define (diff-interval i1 i2)
    (make-interval (- (lower-bound i1) (upper-bound i2))
        (- (upper-bound i1) (lower-bound i2))))

(diff-interval (make-interval 1 2) (make-interval 3 4)) ; (-3 . -1)
(diff-interval (make-interval 3 4) (make-interval 1 2)) ; (1 . 3)
