#lang racket

(define (square-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree)) (* tree tree))
    (else (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

(define (square-tree-with-map tree)
  (if (pair? tree)
        (map (lambda (sub-tree) (square-tree-with-map sub-tree)) tree)
        (* tree tree)))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(square-tree-with-map
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
