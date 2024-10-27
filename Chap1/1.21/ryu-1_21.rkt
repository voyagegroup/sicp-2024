#lang sicp
; 1.21
; smallest-divisor手続きを使い, 次の数の最小除数を見つけよ: 199, 1999, 19999.

(define (smallest-divisor n)
  (find-divisor n 2))


(define (square x)
  (* x x))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
; -> 199
(smallest-divisor 1999)
; -> 1999
(smallest-divisor 19999)
; -> 7
