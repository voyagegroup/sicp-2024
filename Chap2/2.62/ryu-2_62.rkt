#lang sicp

; 与えられた要素が集合の構成要素であるか
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; 引数としてオブジェクトと集合をとり, 元の集合の要素と追加する要素を含む集合を返す
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set) ; 重複の場合
        ((< x (car set)) (cons x set)) ; 追加
        (else (cons (car set) (adjoin-set x (cdr set))))))

; 二つの集合の和集合, つまりどちらかの集合に現れる要素を含んでいる集合を計算する
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 (else
                  (cons x2 (union-set set1 (cdr set2)))))))))

(define A '(1 2 3 ))
(define B '(2 3 4))

(union-set A B) ; -> (1 2 3 4)

; x1 = 1, x2 = 2
; (< x1 x2) -> #t
(cons 1 (union-set '(2 3) '(2 3 4)))
; x1 = 2, x2 = 2
; (= x1 x2)
(cons 1 (cons 2 (union-set '(3) '(3 4))))
; x1 = 3, x2 = 3
; (= x1 x2)
(cons 1 (cons 2 (cons 3 (union-set '() '(4)))))
; (null? set1) -> #t
(cons 1 (cons 2 (cons 3 '(4))))
