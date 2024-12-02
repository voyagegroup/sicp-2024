#lang racket

(define (inc x) (+ x 1))

(define (double f)
    (lambda (x) (f (f x))))

((double inc) 5)
; -> 7

(((double (double double)) inc) 5)
; -> 21

(((double (double double)) inc) 5)
; (((double (lambda (x) (double (double x))) inc) 5)
; (((lambda (y) ((lambda (x) (double (double x)) ((lambda (x) (double (double x)) x))) y)) inc) 5)
; ((lambda (x) (double (double x)) ((lambda (x) (double (double x)) x)) inc) 5)

; (double double) から評価を初めて double 5 となって終わってしまった・・・
