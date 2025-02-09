#lang racket

(define (last-pair li)
  (if (null? (cdr li))
      (list (car li))
      (last-pair (cdr li))))

(last-pair (list 23 72 149 34))