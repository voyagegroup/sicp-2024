#lang sicp

(define (inc x) (+ x 1))
(inc 5)
; -> 6


(define (double f)
  (lambda (x) (f (f x))))


; (double inc)は +2足す処理になる
((double inc) 2)
; -> 4

(((double (double double)) inc) 5)
; -> 21

; A. 21になった