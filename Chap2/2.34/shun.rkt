#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))



(horner-eval 2 (list 1 3))
; (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 0 (list 1 3))
; (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)) 1 (accumulate op 0 (list 3)))
; (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)) 1 (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)) 3 0)))) ;最後はinitialの0が返ってくる
; (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)) 1 (+ 3 (* 2 0)))))
; (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)) 1 3)))
;  (+ 1 (* 2 3))

; 1*x^0 + 3*x^1
; 1*2^0 + 3*2^1
; 1 + 6
; 7

; (... (an x+an-1)x+ ... +a1 ) x+a0
; a0 = this-coeff
; (... (an x+an-1)x+ ... +a1 ) = hegier-terms