#lang racket

(define (fringe items)
  (if (pair? items)
      (if (pair? (car items))
          (append (fringe (car items)) (fringe (cdr items)))
          (cons (car items) (fringe (cdr items))))
      items))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))
