#lang sicp


; --- 本文の手続き
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

; --- ここから
(define (golden-ratio x)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(golden-ratio 1.0)
; -> 1.6180327868852458

(golden-ratio 50.0)
; -> 1.6180327868852458
