#lang sicp
; 問題1.16, 1.17の結果を使い, 加算, 二倍, 二分による, 対数的ステップ数の, 二つの整数の乗算の反復的プロセスを生成する手続きを工夫せよ.
(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (mult-fast-iter a b n)
  (cond ((= b 0) n)
        ((even? b) (mult-fast-iter (double a) (halve b) n))
        (else (mult-fast-iter a (- b 1) (+ a n)))))
          
(define (mult-fast a b)
  (mult-fast-iter a b 0))

(mult-fast 2 4)
(mult-fast-iter 2 4 0)
(mult-fast-iter 4 2 0)
(mult-fast-iter 8 1 0)
(mult-fast-iter 8 0 8)
; -> 8

(mult-fast 2 9)
(mult-fast-iter 2 9 0)
(mult-fast-iter 2 8 2)
(mult-fast-iter 4 4 2)
(mult-fast-iter 8 2 2)
(mult-fast-iter 16 1 2)
(mult-fast-iter 16 0 18)
; -> 18

(mult-fast 3 3)
(mult-fast-iter 3 3 0)
(mult-fast-iter 3 2 3)
(mult-fast-iter 6 1 3)
(mult-fast-iter 6 0 9)
; -> 9




