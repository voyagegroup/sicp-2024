#lang sicp


(define (f g)
  (g 2))

(define (square x) (* x x))

(f square)
; (square 2)
; (* 2 2)
; -> 4

(f (lambda (z) (* z (+ z 1))))
; ->6

(f f)
; 手続きfの本体を取り出す
(g 2)
; 仮パラメタ（g）を実引数（f）に置き換え
(f 2)
; ローカルでgが2になっている
(2 2)
; -> application: not a procedure; expected a procedure that can be applied to arguments given: 2
; 2は手続きじゃないのでエラーになった
