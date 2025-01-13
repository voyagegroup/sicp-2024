#lang racket

(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))


; (car (cons x y))

; (cons x y) を置き換えモデルを使って評価する
; (cons (x y))
; -> (lambda (m) (m x y))

; (cons x y) を置き換えた式を使って評価する
; -> (car (lambda (m) (m x y)))
; -> ((lambda (m) (m x y)) (lambda (p q) p))
; -> ((lambda (p q) p) x y)
; -> x

; 結果は x になる

; cdr の定義
(define (cdr z)
    (z (lambda (p q) q)))
