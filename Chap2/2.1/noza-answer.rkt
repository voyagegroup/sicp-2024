#lang racket

(define (number x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (number x))
    (display "/")
    (display (denom x)))

(define (make-rat n d)
    (let (
        (abs-n (abs n))
        (abs-d (abs d))
        (g (gcd (abs n) (abs d))))
    (if (> (* n d) 0)
        (cons (/ abs-n g) (/ abs-d g))
        (cons (- (/ abs-n g)) (/ abs-d g)))))

(print-rat (make-rat 2 4))
(print-rat (make-rat -2 4))
(print-rat (make-rat 2 -4))
(print-rat (make-rat -2 -4))
