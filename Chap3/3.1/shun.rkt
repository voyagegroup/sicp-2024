#lang sicp

(define (make-accumulator n)
  (let ((num n))
    (lambda (added)
      (begin (set! num (+ num added)) num))))

(define A (make-accumulator 5))

(A 10)
; 15

(A 10)
; 25