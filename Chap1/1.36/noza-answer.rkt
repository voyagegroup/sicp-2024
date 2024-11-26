#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "gess: ")
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

#| 1.35 推測値を順に表示してみる |#
(define (func x) (+ 1 (/ 1 x)))

#| (fixed-point func 1.0) |#

(define (func2 x) (/ (log 1000) (log x)))

(fixed-point func2 2.0)
