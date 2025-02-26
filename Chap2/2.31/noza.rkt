#lang racket

(define (square x) (* x x))

(define (tree-map proc tree)
  (if (pair? tree)
      (map (lambda (sub-tree) (tree-map proc sub-tree)) tree)
      (proc tree)))

(define (square-tree tree)
  (tree-map square tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
