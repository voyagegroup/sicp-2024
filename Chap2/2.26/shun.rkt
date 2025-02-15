#lang racket

(define x (list 1 2 3))

(define y (list 4 5 6))

(append x y) ; '(1 2 3 4 5 6)
; appendはlist同士を結合した新しいlistを生成する

(cons x y) ; '((1 2 3) 4 5 6)
; consはxのリストの後ろにポインタを持ち、それが次のリストの先頭の4を指し示す

(list x y) ; '((1 2 3) (4 5 6))
; listはxのリストの後ろにポインタを持ち、それがyのリストを指し示す

; と思ったが教科書の図を見る限りポインタはどちらも4の位置を指していそう。
