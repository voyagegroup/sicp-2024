#lang sicp

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (make-rat n d)
  (define (combiner)
        (if (or (and (< n 0) (< d 0))
            (and (>= n 0) (>= d 0)))
        +
        -))                     
  (let ((g (gcd n d)))
    (cons (/ (abs n) g) ((combiner) (/ (abs d) g)))))

(make-rat -1 2)
(make-rat 1 2)
(make-rat 1 -2)
(make-rat -1 -2)

; -- output
; (1 . -2)
; (1 . 2)
; (1 . -2)
; (1 . 2)
; --