#lang racket

(define (cube n)
  (* n n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (total old k)
    (cond
      ((> k n) old)
      ((= k 0) (total (y 0) 1))
      ((= k n) (total (+ old (y k)) (+ k 1)))
      ((even? k) (total (+ old (* 2 (y k))) (+ k 1)))
      (else (total (+ old (* 4 (y k))) (+ k 1)))))
  (/ (* h (total 0 0)) 3)
  )

(simpson cube 0 1 100)
(simpson cube 0 1 1000)

; nによらず、1/4となった。

(define (cube n)
  (* n n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)

; 0.24998750000000042

; シンプソンの方が精度が高いことがわかる。