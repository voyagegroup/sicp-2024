#lang sicp

(car ''abracadabra)
; quate


; --- 挙動を見てみる
(cdr ''abracadabra)
; (abracadabra)
(cadr ''abracadabra)
; abracatabra

''abracatabra
; 'abracatabra

''(a)
; '(a)
(car ''(a))
; quote
(cdr ''(a))
; ((a))

(car ''(quate a))
; quate
(car '(quate a))
; quate


; --- 回答

''(abracatabra)
; ''でうしろの'がquateになった
; イメージはこんな感じ
; '(quate abracatabra)