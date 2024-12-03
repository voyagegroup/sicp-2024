#lang sicp

(define (cont-frac n d k combiner)
  (define (cont-iter i result)
    (if (= 0 i)
        result
    (cont-iter (- i 1) (/ (n i) (combiner (d i) result))))) ; (+ (d i) result) → (combiner (d i) result) に変更
  (cont-iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100
           +)
; -> 0.6180339887498948

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x)))
             (lambda (i) (- (* i 2.0) 1.0))
             k
             -))

(tan 0.1)
; -> 0.10033467208545054
(tan-cf 0.1 100)
; -> 0.10033467208545055

(tan 10)
; -> 0.6483608274590867
(tan-cf 10 100)
; -> 0.6483608274590866
