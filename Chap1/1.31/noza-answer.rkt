#lang racket

#| productの実装 |#
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

#| factorialの実装 |#
(define (factorial n)
  (define (next x) (+ x 1))
  (define (term x) x)
  (product term 1 next n))

#| factorialの検算 |#
(factorial 5)
(factorial 4)
(factorial 3)
(factorial 2)


#| pi近似計算の実装 |#
(define (pi-approximation n)
  (define (next x) (+ x 1))
  (define (term x) (* (/ (* 2 x) (- (* 2 x) 1)) (/ (* 2 x) (+ (* 2 x) 1))))
  (* (/ 8 3) (product term 2 next n)))

#| pi近似計算の検算 |#
(exact->inexact (pi-approximation 10000))
#| 3.14151411... |#

#| 反復的プロセスの実装 |#
(define (product-ext term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
