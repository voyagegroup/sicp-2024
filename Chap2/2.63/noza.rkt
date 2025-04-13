#lang racket

(define (entry tree) (car tree))


(define (left-branch tree) (cadr tree))


(define (right-branch tree) (caddr tree))


(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(define (tree1)
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define (tree2)
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))
; -> '(3 (1 () ()) (7 (5 () ()) (9 () ())))
(define (tree3)
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
             '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))
; -> '(5 (3 () ()) (9 (7 () ()) (11 () ())))

(tree->list-1 (tree1))
(tree->list-2 (tree1))

(tree->list-1 (tree2))
(tree->list-2 (tree2))

(tree->list-1 (tree3))
(tree->list-2 (tree3))

; どちらも各要素を左から順に１回ずつ訪れる。
; そのため、
; a, どちらも同じ結果になる。
; b, どちらも計算量は O(n) になる。
