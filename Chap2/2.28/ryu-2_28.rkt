#lang sicp

(define (fringe items)
  (cond ((null? items) (list))
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cdr items))))))

(fringe (list 1 3 (list 5 7)))


(define x (list (list 1 2) (list 3 4)))

(fringe x)
; (1 2 3 4)

(fringe (list x x))
; (1 2 3 4 1 2 3 4)