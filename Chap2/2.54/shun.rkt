#lang racket

(define (equal? li1 li2)
  (cond ((or (null? li1) (null? li2)) #t)
        ((not (eq? (car li1) (car li2))) #f)
        (else (equal? (cdr li1) (cdr li2)))))

(equal? '(this is a list) '(this is a list))
; #t

(equal? '(this is a list) '(this (is a) list))
; #f

(equal? '(this is a list) '(this is a))
; #t
; これは異なるリストではあるが、同じ順に並んだ同じ要素を含む時なので#tで正しい