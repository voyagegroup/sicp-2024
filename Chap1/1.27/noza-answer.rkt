#lang racket

; 動作に必要な関数
(define (square x) (* x x))

; 本文、問題からフェルマーテストのコードをコピー-------
(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))   

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond 
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))
; ここまで---------------------------------------------

; 整数nをとり, a < nなるすべてのaで, a^nがnを法としてaの合同になるかどうか見る手続き
(define (expmod-n-iter n ct)
  (display ct)
  (display ": ")
  (if (< ct n) 
    (begin
      (if (= (expmod ct n n) (remainder ct n)) (display "true") (display "false"))
      (newline)
      (expmod-n-iter n (+ ct 1)))
    0))

(define (expmod-n n)
  (expmod-n-iter n 1))

; カーマイケル数の最小値は561なのでそれで試す
(expmod-n 500)
(expmod-n 561)
(expmod-n 1105)
