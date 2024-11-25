#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(phi)

; 黄金比はφ^2 = φ + 1を満たす。両辺をφで割って、
; φ = 1 + 1/φ を得ることができる。

; 1.6180327868852458