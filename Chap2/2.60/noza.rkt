#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))

;; これでよかった
; (define (union-set set1 set2)
;   (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(union-set '(1 2 3) '(4 5 6))
(union-set '(1 2 3) '(3 4 5))

(intersection-set '(1 2 3) '(4 5 6))
(intersection-set '(1 2 3) '(3 4 5))

;; 計算量について、set1 の要素数を n、set2 の要素数を m とする。
;; element-of-set? は O(n) である。
;; adjoin-set は O(1) である。
;; union-set は O(n) である。
;; intersection-set は O(n * m) である。
;; よって、要素の追加や和集合の計算が多くなるようなケースではこちらの方が有利である。
