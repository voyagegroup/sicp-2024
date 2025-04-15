#lang sicp

; 与えられた要素が集合の構成要素であるか
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; 引数としてオブジェクトと集合をとり, 元の集合の要素と追加する要素を含む集合を返す
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; 二つの集合の和集合, つまりどちらかの集合に現れる要素を含んでいる集合を計算する
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))))

; 二つの集合の積集合, つまり両方の集合に現れる要素だけを含む集合を計算する
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define A '(1 2 3))
(define B '(2 3 4))

(element-of-set? 2 A) ; => #t
(element-of-set? 5 A) ; => #f

(adjoin-set 3 A) ; -> (1 2 3)
(adjoin-set 4 A) ; -> (4 1 2 3)

(union-set A B) ; -> (1 2 3 4)
(intersection-set A B) ; -> (2 3)