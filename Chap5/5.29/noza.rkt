#lang sicp

; 5.26 の末尾再帰を保つ監視つき評価器を使う。
(#%require (file "../5.26/noza.rkt"))

; 起動後に次の定義と (fib 0) から (fib 10) を入力する。
; (define (fib n)
;   (if (< n 2)
;       n
;       (+ (fib (- n 1)) (fib (- n 2)))))
