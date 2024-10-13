#lang racket

; 本文から持ってきた
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter b n a)
  (cond ((= n 1) a)
        ((even? n) (fast-expt-iter b (/ n 2) (* a a)))
        (else (* a (fast-expt-iter b (- n 1) a)))))

(define (fast-expt b n)
  (fast-expt-iter b n b))


(fast-expt 2 10)
(fast-expt 5 3)
(fast-expt 10 11)

