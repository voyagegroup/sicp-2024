#lang sicp

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (simpson-func k)
      (cond 
        ((= k 0) 
         (f a)) ; 初項
        ((= k n)
         (f (+ a (* k h)))) ; 最後の項
        ((even? k) 
         (* 2.0 (f (+ a (* k h))))) ; 偶数番目の項
        (else 
         (* 4.0 (f (+ a (* k h))))))) ; 奇数番目の項
    
    (* (/ h 3.0)
       (sum simpson-func 0 inc n)))) ; 0 から n まで合計 して、 h/3をかける

(simpson cube 0 1 100)
; -> 0.24999999999999992
; yosasou
