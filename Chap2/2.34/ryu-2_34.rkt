#lang sicp
; https://www.math.tsukuba.ac.jp/~terui/_media/%E8%A8%88%E7%AE%97%E6%A9%9F%E6%95%B0%E5%AD%A6i_2019_%E7%AC%AC5%E5%9B%9E.pdf

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))



; x=2, 1 + 3x + 0x^2 + 5x^3 + 0x^4 + x^5
(horner-eval 2 (list 1 3 0 5 0 1))
; 79

; x=2, 1 + 2x + 3x^2
(horner-eval 2 (list 1 2 3))
(accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
              0
              (list 1 2 3))
((lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
 (car (list 1 2 3))
 (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 (cdr (list 1 2 3))))

((lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
 1
 (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 (list 2 3)))


; (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 (list 2 3))
((lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
 2
 (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 (list 3)))

; (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 3)
((lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
 3
 (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 (list)))

; (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 (list))
; (null? (list)) -> #t
0

; 戻っていく
; (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 3)
((lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
 3
 0)
(+ (* 0 2) 3)
; -> 3

; (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff)) 0 (list 2 3))
((lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
 2
 3)
(+ (* 3 2) 2)
; -> 8

((lambda (this-coeff higher-terms) (+ (* higher-terms 2) this-coeff))
 1
 8)
(+ (* 8 2) 1)
; -> 17