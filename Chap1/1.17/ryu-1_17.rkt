#lang sicp
; 本節のべき乗アルゴリズムは, 乗算の繰返しによるべき乗の実行に基づいていた. 同様に整数の乗算を加算の繰返しで実行出来る. 次の乗算手続きは(この言語には加算はあるが, 乗算はないと仮定する)expt手続きに似たものである:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
; このアルゴリズムはbに線形のステップ数をとる. 加算の他に整数を二倍するdouble演算と(偶数の)整数を2で割るhalve演算もあるとしよう. これらを使い, fast-exptと類似の対数的ステップ数の乗算手続きを設計せよ.

(* 4 2)
; -> (= 2 0) -> #f
(+ 4 (* 4 (- 2 1)))
(+ 4 (* 4 1))
; -> (= 1 0) -> #f
(+ 4 (+ 4 (* 4 (- 1 1))))
(+ 4 (+ 4 (* 4 0)))
; -> (= 0 0) -> #t
(+ 4 (+ 4 0))
(+ 4 4)
; -> 8

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (fast-mult a b)
 (cond ((= b 0) 0)
       ((even? b) (fast-mult (double a) (halve b)))
       (else (+ a (fast-mult a (- b 1))))))

(fast-mult 4 2)
; -> (= 2 0) #f
; -> (even? 4) #t
(fast-mult (double 4) (halve 2))
(fast-mult 8 1)
; -> (= 1 0) #f
; -> (even? 1) #f
(+ 8 (fast-mult 8 (- 1 1)))
(+ 8 (fast-mult 8 0))
; -> 8

(fast-mult 2 8)
(fast-mult 4 4)
(fast-mult 8 2)
(fast-mult 16 1)
(+ 16 (fast-mult 16 0))
(+ 16 0)
; -> 16

(fast-mult 3 10)
(fast-mult 6 5)
(+ 6 (fast-mult 6 4))
(+ 6 (fast-mult 12 2))
(+ 6 (fast-mult 24 1))
(+ 6 (+ 24 (fast-mult 24 0)))
(+ 6 (+ 24 0))
; -> 30

