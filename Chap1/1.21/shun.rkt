; smallest-divisor手続きを使い, 次の数の最小除数を見つけよ: 199, 1999, 19999.

; 199
; A. 199

; 1999
; A. 1999

; 19999
; A. 7

#lang racket
(define (square n)
  (* n n))

(define (divides? x y)
  (= (remainder y x) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))