#lang racket

(define us-coins (list 50 25 10 5 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? values)
  (null? values))

(define (except-first-denomination values)
  (cdr values))

(define (first-denomination values)
  (car values))

(cc 100 us-coins)

(define us-coins-other (list 5 10 1 25 50))

(cc 100 us-coins-other)
