#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* c (/ p 100)) 2)))
    (make-center-width c w)))