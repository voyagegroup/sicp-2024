#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set '(1 2 3) '(2 4 5))
; '(1 3 2 4 5)

(union-set '(1 2 3) '())
; '(1 2 3)

(union-set '() '(1 2 3))
; '(1 2 3)

(union-set '() '())
; '()