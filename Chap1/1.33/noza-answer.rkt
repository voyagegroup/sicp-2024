#lang racket

#| prime? の実装 |#
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (< n 2)
    false
    (= n (smallest-divisor n))))
#| ここまで |#

#| a. |#
(define (accumulate-filter combiner filter null-value term a next b)
  (cond
    ((> a b) null-value)
    ((filter a) (combiner (term a) (accumulate-filter combiner filter null-value term (next a) next b)))
    (else (accumulate-filter combiner filter null-value term (next a) next b))))

(define (sum-square-prime a b)
  (accumulate-filter + prime? 0 square a add1 b))

(sum-square-prime 1 10) ; => 87


#| b. |#
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (product-gcd a b)
  (define (gcd1 i)
    (if (= (gcd i b) 1)
      true
      false))
  (accumulate-filter * gcd1 1 identity a add1 b))

; identiry は恒等関数

(product-gcd 1 10) ; => 189
