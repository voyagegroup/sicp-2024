#lang racket

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (can-divide-count base divisor count)
  (if (= (remainder base divisor) 0)
      (can-divide-count (/ base divisor) divisor (+ count 1))
      count))

(define (car z)
  (can-divide-count z 2 0))

(define (cdr z)
  (can-divide-count z 3 0))


(define x (cons 8 81))

(display (car x))
(newline)
(display (cdr x))

; 8
; 81