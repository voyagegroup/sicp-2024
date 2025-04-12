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


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))

(union-set '(1) '(2 3))
; '(1 2 3)

(union-set '(4) '(2 3))
; '(2 3 4)

(union-set '(3) '(2 3 4))
; '(2 3 4)

(union-set '(3 5) '(2 4 6))
; '(2 3 4 5 6)

; 集合の要素は毎回必ずset1かset2のどちらかが結果のリストに入るので、集合の要素数nによってステップ数は決まる