#lang racket

; expmodのコピー
(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
      (remainder (square (expmod base (/ exp 2) m)) m))
    (else
      (remainder (* base (expmod base (- exp 1) m)) m))))

; fast-expt 本文に乗ってたやつ
(define (fast-expt b n)
  (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

; 問題で置き換えてたやつ
(define (expmod-ext base exp m)
  (remainder (fast-expt base exp) m))

; (expmod 2 8 8) と (expmod-ext 2 8 8) を置き換えモデルで比較してみる

;(expmod 2 8 8)
;-> (remainder (square (expmod 2 (/ 8 2) 8)) 8)
;-> (remainder (square (expmod 2 4 8) 8))
;-> (remainder (square (remainder (square (expmod 2 2 8)) 8)) 8)
;-> (remainder (square (remainder (square (remainder (square (expmod 2 1 8)) 8)) 8)) 8)
;-> (remainder (square (remainder (square (remainder (square (remainder (* 2 (expmod 2 0 8)) 8)) 8)) 8)) 8)
;-> (remainder (square (remainder (square (remainder (square (remainder (* 2 1) 8)) 8)) 8)) 8)
;-> (remainder (square (remainder (square (remainder (square (remainder 2 8)) 8)) 8)) 8)
;-> (remainder (square (remainder (square (remainder (square 2) 8)) 8)) 8)
;-> (remainder (square (remainder (square (remainder 4 8)) 8)) 8)
;-> (remainder  8)
;-> (remainder 0 8)
;-> 0

;(expmod-ext 2 8 8)
;-> (remainder (fast-expt 2 8) 8)
;-> (remainder (square (fast-expt 2 (/ 8 2))) 8)
;-> (remainder (square (fast-expt 2 4)) 8)
;-> (remainder (square (square (fast-expt 2 (/ 4 2)))) 8)
;-> (remainder (square (square (fast-expt 2 2))) 8)
;-> (remainder (square (square (square (fast-expt 2 (/ 2 2))))) 8)
;-> (remainder (square (square (square (fast-expt 2 1)))) 8)
;-> (remainder (square (square (square (* 2 (fast-expt 2 (- 1 1)))))) 8)
;-> (remainder (square (square (square (* 2 1)))) 8)
;-> (remainder (square (square 4)) 8)
;-> (remainder (square 16) 8)
;-> (remainder 256 8)
;-> 0

; expmodとexpmod-extの違いは都度remainderを用いてmodを求めるか先にaのn乗を求めて最後にmodを求めるかの違いである
; aのn乗が大きい数字になるときexpmod-extはオーバーフローするので、条件によっては高速素数テストと同じには使えない



