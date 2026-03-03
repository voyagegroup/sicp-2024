#lang sicp

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; 本文
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))

    ; dyは未定義だけど、delayで遅延評価されるので、yは定義できる
    (set! y (integral (delay dy) y0 dt))

    ; yは定義済みなので、dyも定義できる
    (set! dy (stream-map f y))

    ; (delay dy) が遅延評価される時には、dyが定義済みなので、yの実数がちゃんととれる
    y))



; 4.18のやつ
(define (solve f y0 dt)
  (let ((y '*unssigned*)
        (dy '*unssigned*))
    (let ((a (integral (delay dy) y0 dt)) ; dyは遅延評価なので、aが定義される
          (b (stream-map f y))) ; まだ、yが*unssigned*なのでエラーになる
      (set! y a)
      (set! dy b))
    y))

