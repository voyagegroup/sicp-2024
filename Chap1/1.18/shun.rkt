#lang racket

(define (double n)
  (* 2 n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-multi a b)
  (fast-multi-iter a b 0))

(define (fast-multi-iter a b prev)
  (cond ((= b 0) prev)
        ((even? b) (fast-multi-iter (double a) (halve b) prev))
        (else (fast-multi-iter a (- b 1) (+ a prev))))
  )

(fast-multi 2 4)