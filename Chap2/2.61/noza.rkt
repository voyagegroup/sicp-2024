#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) '())
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 3 '(1 2 4 5))
; -> '(1 2 3 4 5)
(adjoin-set 3 '(1 2 3 4 5))
; -> '(1 2 3 4 5)

;; 追加したい要素がすでに含まれている場合には、set をそのまま返す。
;; 追加したい要素が含まれていない場合は、追加したい要素より大きい要素が見つかった時点でその直前に追加したい要素を挿入すれば良い。
;; この場合、element-of-set? の時の考えて同様に最悪の計算時間は set の要素数 n になるが、要素の数字が小さい場合リストの先頭に近いところで
;; 探索を止めることができるので平均は n/2 になる。
