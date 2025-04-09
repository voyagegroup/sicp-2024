#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; ここから解答
;; 方針
;; set1 の n 番目の要素について、
;; n + 1 番目までの要素と set2 の union-set ができていれば、その set の中に n 番目の要素が含まれているかを
;; チェックして、含まれていない場合は、adjoin-set で追加する。

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))

(union-set '(1 2 3) '(4 5 6))
(union-set '(1 2 3) '(3 4 5))
