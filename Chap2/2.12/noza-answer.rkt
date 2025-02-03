#lang racket

(define (make-interval a b)
    (cons a b))

(define (lower-bound i)
    (car i))

(define (upper-bound i)
    (cdr i))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c t)
    (make-interval (- c (* c (/ t 100))) (+ c (* c (/ t 100)))))

(define (percent i)
    (* (/ (width i) (center i) ) 100))

(make-center-percent 6.8 10); (9.0 . 11.0)

(percent (make-center-percent 6.8 10)); 0.14705882352941177
