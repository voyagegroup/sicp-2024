#lang racket

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; '(1 (4 (9 16) 25) (36 49))

; mapはcarに対してprocを適用して、cdrに対して基底でなければ再帰的な処理を行う