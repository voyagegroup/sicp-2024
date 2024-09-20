#lang racket

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-iter guess pre-guess x)
  (if (good-enough? guess pre-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess x)))
                 
(define (good-enough? guess pre-guess)
  (< (abs (- guess pre-guess)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))


;大きな数を使った場合、今まではsquareで2乗をしていたため、intの上限をオーバーしてしまっていた。
;小さな数を使った場合、0.001よりも小さな数を使った場合、そこでループが止まってしまう。
;0.001みたいな絶対値ではなく、例えば割合を使うと大きい数でも小さい数でも同じように収束する。
;浮動小数点数の特性からいくと、割合でやるとうまくいく。網目が大きすぎる。
;割合なら指数部を無視できる。仮数部だけで比較できる。
;大きい桁では、大きい数になった時にintの上限を超えた時に丸め込まれてしまい、最後の下一桁が丸め込みによって変わらないので、収束しない。