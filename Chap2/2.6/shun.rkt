#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
; (add-1 (lambda (f) (lambda (x) x)))
; (lambda (f) (lambda (x) (f ( ( (lambda (f) (lambda (x) x)) f) x))))
; ((lambda (f) (lambda (x) x)) f)を評価する
; (lambda (f) (lambda (x) (f ( ((lambda (x) x)) x))))
; ((lambda (x) x) x))を評価する
; (lambda (f) (lambda (x)(f x)))

(define one (lambda (f) (lambda (x)(f x))))

(add-1 one)
; (add-1 (lambda (f) (lambda (x)(f x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x)(f x))) f) x))))
; ((lambda (f) (lambda (x)(f x))) f)を評価する
; (lambda (f) (lambda (x) (f ((lambda (x)(f x)) x))))
; ((lambda (x)(f x)) x)を評価する
; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(plus one two)
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x)(f x))) f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x)(f x))) f) ((lambda (x) (f (f x))) x))))
; (lambda (f) (lambda (x) (((lambda (f) (lambda (x)(f x))) f) (f (f x)))))
; (lambda (f) (lambda (x) ((lambda (x)(f x)) (f (f x)))))
; (lambda (f) (lambda (x) (f (f (f x)))))
