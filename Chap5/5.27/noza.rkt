#lang sicp

; 5.26と同じ監視つき評価器を起動する。
(#%require (file "../5.26/noza.rkt"))

; 問題 5.27: 起動後、以下の式をコメント記号を除いて順に入力する。
;
; (define (factorial n)
;   (if (= n 1)
;       1
;       (* (factorial (- n 1)) n)))
;
; (factorial 1)
; (factorial 2)
; (factorial 3)
; (factorial 4)
; (factorial 5)
; (factorial 10)
;
; 定義時の統計は除き、各 factorial 呼び出しの total-pushes と
; maximum-depth を記録する。
