#lang racket

(define (cont-frac n d k)
    (define (cont-frac-iter i)
        (if (> i k)
            (d k)
            (/ (n i) (+ (d i) (cont-frac-iter (+ i 1))))))
    (cont-frac-iter 1))

(define (tan-cf x k)
    (cont-frac
        (lambda (i)
            (if (= i 1)
                x
                (- (* x x))))
        (lambda (i) (- (* 2 i) 1))
        k))

(tan-cf (/ pi 6) 10)
; -> 0.5773502691896257
(tan-cf (/ pi 4) 10)
; -> 1.0
(tan-cf (/ pi 3) 10)
; -> 1.7320508075688765

; 近似値としては近いものが取れている
