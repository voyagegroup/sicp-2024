#lang racket

(define (reverse li)
  (define (reverse-iter result li)
    (if (null? li)
        result
        (reverse-iter (append (list (car li)) result) (cdr li))))
  (reverse-iter '() li))

(reverse (list 1 4 9 16 25))
