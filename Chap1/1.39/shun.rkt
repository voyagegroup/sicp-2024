#lang racket

(define (cont-frac n d k)
  (define (hoge i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (hoge (+ i 1))))))
  (hoge 1))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))

(tan-cf 1.0 20)

; 1.557407724654902