#lang racket

(define (square n)
  (* n n))

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          ((eq? op 'real-part)
           (* (sqrt (+ (square x) (square y))) (cos (atan y x))))
          ((eq? op 'imag-part) (* (sqrt (+ (square x) (square y))) (sin (atan y x))))
          (else
           (error "Unknown op -- MAKE-FROM-MAG_ANG" op))))
  dispatch)