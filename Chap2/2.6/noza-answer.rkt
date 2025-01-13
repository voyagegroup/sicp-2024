#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

; 0 に 1 を足す
; (add-1 zero)
; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
; (lambda (f) (lambda (x) (f x))) = 1

(define one (lambda (f) (lambda (x) (f x))))

; 1 に 1 を足す
; (add-1 one)
; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g y))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
; (lambda (f) (lambda (x) (f (f x))))

; よって

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus m n)
    (lambda (f) (lambda (x) (m f ((n f) x)))))

(plus one two)
(lambda (f) (lambda (x) ((lambda (g) (lambda (y) (g y)) f) (((lambda (h) (lambda (z) (h (h z)))) f) x))))
(lambda (f) (lambda (x) ((lambda (g) (lambda (y) (g y)) f) ((lambda (z) (f (f z)) x)))))
(lambda (f) (lambda (x) ((lambda (g) (lambda (y) (g y)) f) (f (f x)))))
(lambda (f) (lambda (x) ((lambda (y) (f y)) (f (f x)))))
(lambda (f) (lambda (x) (f (f (f x)))))
