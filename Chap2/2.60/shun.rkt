#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(adjoin-set 1 '(2 3 4))

(define (union-set set1 set2)
  (append set1 set2))
; '(1 2 3 4)

(union-set '(1 2 3) '(2 4 5))
; '(1 3 2 4 5)

(define (exclude-of-set x set)
  (cond ((null? set) '())
        ((equal? x (car set)) (cdr set))
        (else (cons (car set) (exclude-of-set x (cdr set))))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) (exclude-of-set (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

(intersection-set '(1 3 3 5 2) '(2 3 4 5 3 5 5))
; '(3 3 5 2)
; 登場回数分だけの積集合になっている


; intersection-setでは除外する分余計に計算が走る。
; それ以外は余計な計算をしない分早くなった。

; 応用例は浮かばなかった...