#lang racket

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter old a)
    (cond
      ((> a b) old)
      ((filter a) (iter (combiner old (term a)) (next a)))
      (else (iter old (next a)))
      ))
  (iter null-value a))


; 区間a, bの素数の二乗の和(prime?述語は持っていると仮定する.)
(define (inc n)
  (+ n 1))

(define (sum a b)
  (+ a b))

(define (square n)
  (* n n))

(define (divides? x y)
  (= (remainder y x) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (prime? n)
  (if (< n 2)
      false
      (= n (smallest-divisor n))
  )
)

(define (square-total a b)
  (filtered-accumulate sum 0 square a inc b prime?))

(square-total 1 6)
; 38

; nと互いに素で, nより小さい正の整数(つまりi < nでGCD(i, n)=1なる全整数i)の積
(define (identity x) x)

(define (inc n)
  (+ n 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prime-multi-total n)
  (filtered-accumulate * 1 identity 1 inc n (lambda (i) (= (gcd i n) 1))))

(prime-multi-total 5)
; 24