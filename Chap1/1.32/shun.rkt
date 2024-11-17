#lang racket

; iter
(define (accumulate combiner null-value term a next b)
  (define (iter old a)
    (if (> a b)
        old
        (iter (combiner old (term a)) (next a))))
  (iter null-value a))

(define (multi a b)
  (* a b))

(define (a-factorial n)
  (accumulate multi 1 identity 1 inc n))

(a-factorial 3)
; 6

; recursive
(define (accumulate combiner null-value term a next b)
(if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b)))
)

(a-factorial 3)
; 6