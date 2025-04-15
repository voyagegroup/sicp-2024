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


(define A '(1 2 3 ))
(define B '(2 3 4))

(element-of-set? 2 A) ; => #t
(element-of-set? 5 A) ; => #f

(adjoin-set 3 A) ; -> (1 2 3)
(adjoin-set 4 A) ; -> (1 2 3 4)

(define C '(1 3 4 5))
(adjoin-set 2 C) ; -> (1 2 3 4 5)
(adjoin-set 0 C) ; -> (0 1 3 4 5)
(adjoin-set 100 C) ; -> (1 3 4 5 100)

; 順序をもつことで途中で処理が打ち切れる。