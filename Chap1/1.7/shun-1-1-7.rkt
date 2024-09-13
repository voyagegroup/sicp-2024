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