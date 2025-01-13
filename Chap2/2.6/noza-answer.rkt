#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

; 0 に 1 を足す
; (add-1 zero)
; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
; (lambda (f) (lambda (x) (f (lambda (x) x)))) = 1

(define one (lambda (f) (lambda (x) (f (lambda (x) x)))))
; (lambda (g) (lambda (y) (g (lambda (y) y))))

; 1 に 1 を足す
; (add-1 one)
; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g (lambda (y) y)))) f) x)))))
; (lambda (f) (lambda (x) (f ((lambda (y) (f (lambda (y) y))) x))))
; (lambda (f) (lambda (x) (f (f (lambda x) x))))

; 計算用メモ
; ((lambda (g) (lambda (y) (g (lambda (y) y)))) f)
;((lambda (g) (lambda (y) (g (lambda (y) y)))) f)
(lambda (y) (f (lambda (y) y)))

; ((lambda (y) (f (lambda (y) y))) x)
;((lambda (y) (f (lambda (y) y))) x)
;(f (lambda x) x)

; よって

(define one (lambda (f) (lambda (x) (f (lambda (x) x)))))
(define two (lambda (f) (lambda (x) (f (f (lambda x) x)))))

; おそらく

(define three (lambda (f) (lambda (x) (f (f (f (lambda x) x))))))
