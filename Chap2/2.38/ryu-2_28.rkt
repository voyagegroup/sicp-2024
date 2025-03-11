#lang sicp
; --- right

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (op (car rest) (iter result (cdr rest)))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
; (/ 1 (iter 1 (list 2 3)))
; (/ 1 (/ 2 (iter 1 (list 3))))
; (/ 1 (/ 2 (/ 3 (iter 1 (list)))))
; (/ 1 (/ 2 (/ 3 1)))
; 1 1/2

(fold-right list nil (list 1 2 3))
; (1 (2 (3 ())))

; --- left

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))



(fold-left / 1 (list 1 2 3))
; (iter (/ 1 1) (list 2 3))
; (iter (/ 1 2) (list 3))
; (iter (/ 1/2 3) (list)))
; -> 1/6

(fold-left list nil (list 1 2 3))

; (((() 1) 2) 3)

; --- fold-rightとfold-leftが, どのような並びに対しても同じ値を生じるためにopが満たすべき性質は何か.

; 足し算やかけ算のような、処理の順序を気にしないようなop
(fold-right + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))
(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
