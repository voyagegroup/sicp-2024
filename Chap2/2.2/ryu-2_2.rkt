#lang sicp
; 線分の表現を定義する構成子
(define (make-segment start end)
  (cons start end))

; 選択子
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; 点は一対の数: x座標とy座標で表現する構成子
(define (make-point x y) (cons x y))

; 選択子
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; 選択子と構成子を使い, 引数として線分をとり, 中間点(座標が両端点の座標の平均である点)を返す手続き
(define (average x y) (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((midpoint-x (average (x-point (start-segment segment))
                             (x-point (end-segment segment))))
        (midpoint-y (average (y-point (start-segment segment))
                             (y-point (end-segment segment)))))
    (make-point midpoint-x midpoint-y)))

; 確認の手続き
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


; 構成子を作る
(define start-point (make-point 1 2))
(define end-point (make-point 5 6))
(define my-segment (make-segment start-point end-point))

; 確認
(x-point (start-segment my-segment))
; -> 1
(y-point (end-segment my-segment))
; -> 6

; midpoint
(define my-midpoint (midpoint-segment my-segment))
(print-point my-midpoint)
; -> (3,4)
