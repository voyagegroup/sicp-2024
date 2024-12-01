#lang racket

; a

(define (cont-frac n d k)
    (define (cont-frac-iter i)
        (if (> i k)
            (d k)
            (/ (n i) (+ (d i) (cont-frac-iter (+ i 1))))))
    (cont-frac-iter 1))

; (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11))
; -> 1.61797

; (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11))
; -> 1.61805

; 黄金比は 1.6180... なので k = 11 で 4 桁の精度の近似を得ることができる

; b

; 再起プロセスを生成するので反復プロセスを生成するものを実装する

(define (cont-frac-ext n d k)
    (define (cont-frac-iter i result)
        (if (> 1 i)
            result
            (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
    (cont-frac-iter k (/ (n k) (d k))))


(/ 1 (cont-frac-ext (lambda (i) 1.0) (lambda (i) 1.0) 11))
(/ 1 (cont-frac-ext (lambda (i) 1.0) (lambda (i) 1.0) 10))

; cont-frac と cont-frac-ext は同じ結果を返すので実装としてあっていそう
