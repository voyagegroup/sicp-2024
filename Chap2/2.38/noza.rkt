#lang racket

(define (fold-right op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))

(fold-left / 1 (list 1 2 3))

(fold-right list null (list 1 2 3))

(fold-left list null (list 1 2 3))

; Q.fold-rightとfold-leftが, どのような並びに対しても同じ値を生じるためにopが満たすべき性質は何か.
; A. (op a b) = (op b a) が成立するようなopの時、fold-rightとfold-leftが同じ値を生じる。

(fold-right * 1 (list 1 2 3))
;1 1/2

(fold-left * 1 (list 1 2 3))
;1/6
