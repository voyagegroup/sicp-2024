#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-multi-iter a b x)
  (cond 
    ((= b 1) (+ a x))
    ((even? b) (fast-multi-iter (* a 2) (/ b 2) x))
    (else (fast-multi-iter a (- b 1) (+ x a)))))

(define (fast-multi a b) (fast-multi-iter a b 0))

(fast-multi 2 11)
(fast-multi 6 9)

