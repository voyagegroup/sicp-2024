#lang racket

(define (cont-frac n d k)
  (define (hoge i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (hoge (+ i 1))))))
  (hoge 1))


(define (e)
  (+ 2 (cont-frac (lambda (i) 1.0)
           (lambda (i) (if (= (remainder i 3) 2)
                           (+ (* (quotient i 3) 2) 2)
                           1))
           20)))

(e)
; 2.718281828459045