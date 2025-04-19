#lang racket

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2))) ; topを除いた分を右辺と左辺の2で割ってサイズを求める
        (let ((left-result (partial-tree elts left-size))) ; 左の結果の木を作る
          (let ((left-tree (car left-result)) 
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts)) ; リストの真ん中、non-left-eltsの最初の要素を取得
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result))) ; 残りをまとめる
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
'(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

; a
; 木の深さが0になるまで左右の木を解釈し続けて、結果の木を返す

;    5
;  /   \
; 1     9 
;  \   / \
;   3 7  11

; b
; 木の深さnの分だけpartial-treeを行うため、&Theta(n)