#lang racket

(define (add1 x) (+ x 1))

(define (cube x) (* x x x))

; a.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b)) 

; b.
(define (accumulate-ext combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result)))) (iter a null-value))

(define (sum-ext term a next b)
  (accumulate-ext + 0 term a next b))

(define (product-ext term a next b)
  (accumulate-ext * 1 term a next b))

; 検算

(sum cube 1 add1 3) ; => 36
(sum-ext cube 1 add1 3) ; => 36
(sum cube 2 add1 4) ; => 99
(sum-ext cube 2 add1 4) ; => 99

(product cube 1 add1 3) ; => 216
(product-ext cube 1 add1 3) ; => 216
(product cube 2 add1 4) ; => 13824
(product-ext cube 2 add1 4) ; => 13824
