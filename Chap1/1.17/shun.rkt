#lang racket

(define (double n)
  (* 2 n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))


(define (fast-multi a b)
  (cond ((= b 0) 0)
  ((even? b) (double (fast-multi a (halve b))))
  (else (+ a (fast-multi a (- b 1))))))


(fast-multi 2 4)