#lang sicp

; --- もともと
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))


(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


; ---- 2.63 サンプルコード
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

; ---- 実行結果

(define tree-A
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))


(define tree-B
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))

(define tree-C
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

tree-A
(tree->list-1 tree-A)
(tree->list-2 tree-A)

tree-B
(tree->list-1 tree-B)
(tree->list-2 tree-B)

tree-C
(tree->list-1 tree-C)
(tree->list-2 tree-C)

#|: output
(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))
(1 3 5 7 9 11)
(1 3 5 7 9 11)
(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))
(1 3 5 7 9 11)
(1 3 5 7 9 11)
(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))
(1 3 5 7 9 11)
(1 3 5 7 9 11)
|#

; ---- 回答

; a. 二つの手続きはすべての木に対して同じ結果を生じるか. そうでなければ, 結果はどう違うか. 二つの手続きは図2.16のような木からどういうリストを生じるか.
; 同じ結果

; b. n個の要素の釣合っている木をリストに変換するのに必要なステップ数の増加の程度は, 二つの手続きで同じか. 違うなら, どちらがより遅く増加するか.
; list-1
; 再帰でappendが実行される
; list-2
; consでを追加していくだけ
; list-1のほうが遅い