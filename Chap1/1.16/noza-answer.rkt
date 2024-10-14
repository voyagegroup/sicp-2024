#lang racket

; 本文から持ってきた
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter b n a)
  (display b)
  (display " ")
  (display n)
  (display " ")
  (display a)
  (newline)
  (cond ((= n 1) (* a b))
        ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))


(fast-expt 2 10)
(newline)
(fast-expt 2 11)
(newline)
(fast-expt 5 3)
(newline)
(fast-expt 10 11)

