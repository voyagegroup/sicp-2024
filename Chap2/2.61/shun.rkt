#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(adjoin-set 1 '(2 3 4)) 
; '(1 2 3 4)

(adjoin-set 3 '(2 3 4))
; '(2 3 4)

(adjoin-set 5 '(2 3 4))
; '(2 3 4 5)

