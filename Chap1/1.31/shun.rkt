#lang racket

(define (product f a next b)
  (if (> a b)
  1
  (* (f a) (product f (next a) next b))))

(define (inc n)
  (+ n 1))

(define (identity x) x)

; factorial
(define (factorial n)
  (product identity 1 inc n))

(factorial 3)
; 6

; pi
(define (pi-inc n)
  (+ n 2))

(define (pi-f n)
  (/ (* (- n 1) (+ n 1)) (* n n)))

(define (pi n)
  (* 4 (product pi-f 3.0 pi-inc n)))

(pi 1000)

;3.143163842419204

; iter
(define (product f a next b)
  (define (iter old a)
    (if (> a b)
          old
          (iter (* old (f a)) (next a))))
  (iter 1 a))

; 再帰プロセスと同じ結果が得られた。

