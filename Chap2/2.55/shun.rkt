#lang racket

(car ''abracadabra)
; 'quote

; 注34にある通り、解釈系において'aは(quote a)と解釈される。
; つまり(car ''abracadabra)は(quote (quote abracadabra))と表すことができる。
; ここに対してcarを行うのでquoteが得られる