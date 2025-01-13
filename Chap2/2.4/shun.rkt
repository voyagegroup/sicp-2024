#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define x (cons 2 3))

(display (cdr x))
; 3

; (cdr (cons 2 3))
; (cdr (lambda (m) (m 2 3)))
; ((lambda (m) (m 2 3)) (lambda (p q) q))
; mを(lambda (p q) q)で置き換える
; (lambda (2 3) 3)
; 3

