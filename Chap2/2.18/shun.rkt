#lang racket

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (last-pair li)
  (if (null? (cdr li))
      (list (car li))
      (last-pair (cdr li))))

(define (reverse li)
  (define (reverse-iter result li)
    (if (null? li)
        result
        (reverse-iter (append (list (car li)) result) (cdr li))))
  (reverse-iter '() li))

(reverse (list 1 4 9 16 25))
