#lang racket

; cont-frac の実装を持ってくる
(define (cont-frac n d k)
    (define (cont-frac-iter i)
        (if (> i k)
            (d k)
            (/ (n i) (+ (d i) (cont-frac-iter (+ i 1))))))
    (cont-frac-iter 1))

(define (d-i i)
    (let ((mod-3 (modulo i 3)))
        (if (= mod-3 2)
            (* 2 (quotient (+ i 1) 3))
            1)))

; e 2.718281828459045235360287471352
(+ (cont-frac (lambda (i) 1.0) d-i 1000) 2)
; -> 2.7182818284590455
; 割と近似できている
