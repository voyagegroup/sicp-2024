#lang racket

(define (make-point x y)
    (cons x y))

(define (x-point p)
    (car p))

(define (y-point p)
    (cdr p))

(define (make-segment start end)
    (cons start end))

(define (start-segmant segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (midpoint-segment segment)
    (let (
        (start (start-segmant segment))
        (end (end-segment segment)))
        (make-point (/ (+ (x-point start) (x-point end)) 2)
                    (/ (+ (y-point start) (y-point end)) 2))))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 3 4))))
