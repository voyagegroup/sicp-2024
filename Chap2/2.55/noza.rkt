#lang racket

(car ''abracadabra)
; => 'quote

(car ''a)
; => 'quote

(display 'abracadabra)
; => abracadabra
(newline)

(display ''abracadabra)
; => (quote abracadabra)
(newline)

(display '''abracadabra)
; => (quote (quote abracadabra))
(newline)

; ''quote は (quote abracadabra) と評価されるため car で最初の要素を取得すると 'quote が表示される
