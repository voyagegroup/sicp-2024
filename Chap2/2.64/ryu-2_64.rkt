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

; --- 2.64サンプルコード

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
; -> (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
(car (partial-tree '(1 3 5 7 9 11) (length '(1 3 6 7 9 11))))
#|
(partial-tree '(1 3 5 7 9 11) 6)
left-size = (quotient (- 6 1) 2) = (quotient 5 2) = 2
left-result = (partial-tree '(1 3 5 7 9 11) 2)
left-size = (quotient 1 2) = 0
left-result = (partial-tree '(1 3 5 7 9 11) 0)
-> (cons '() '(1 3 5 7 9 11))
this-entry = 1
right-size = 1

right-result = (partial-tree '(3 5 7 8 11) 1)
left-size = 0

left-tree = ()
this-entry = 1
right-tree = (3 () ())

---
left-result = (1 () (3 () ()))
remaining = (5 7 9 11)
---

left-tree = (1 () (3 () ()))
this-entry = 5
right-result = (partial-tree '(7 9 11) 3)

---
(partial-tree '(7 9 11) 3)
left-size=1
left-result = (partial-tree '(7 9 11) 1)

(partial-tree '(7 9 11) 1)
left-size=0
left-result = (partial-tree '(7 9 11) 0) → (() 7 9 11)
this-entry = 7
right-size = 0
right-result = (partial-tree '(9 11) 0) → (() 9 11)
this-entry = 7
left-result=(7 () ())

right-result = (partial-tree '(11) 1)
right-result = (11 () ())

(9 (7 () ()) (11 () ()))
---

(make-tree this-entry left right)

this-entry = 5
left= (1 () (3 () ()))
right=(9 (7 () ()) (11 () ()))

(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
|#


; (tree->list-1 (list->tree '(1 3 5 7 9 11)))
; 釣合の取れた二分木を作る。
; 真ん中を取ってきて、真ん中と右と左に分けて繰り返す。
; O(n)


















