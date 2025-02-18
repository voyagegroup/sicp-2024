#lang sicp

(define x (list 1 2 3))

(define y (list 4 5 6))

; --- 実行するまえの考えるフェーズ ---

; (append x y)
; (1 2 3 4 5 6)

; (cons x y)
; ((1 2 3) (4 5 6))

; (list x y)
; ((1 2 3) (4 5 6))

; --- 実行してみる ---

(append x y)
; (1 2 3 4 5 6
; -> あってた

(cons x y)
; ((1 2 3) 4 5 6)
; -> まちがってた

(list x y)
; ((1 2 3) (4 5 6))
; -> あってた