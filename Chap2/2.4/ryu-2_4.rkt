#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 6 3))
; cons の手続きの本体を取り出し、仮引数を置き換える
(car (lambda (m) (m 6 3)))
; car の手続きの本体を取り出し、仮引数を置き換える
((lambda (m) (m 6 3)) (lambda (p q) p))
((lambda (p q) p) 6 3)
; -> 6

; cdr は ((p q) p) を ((p q) q) にすればよいだけ

(define (cdr z)
  (z (lambda (p q) q)))
