#lang racket

(define (reverse items)
  (define (reverse-iter l1 l2)
    (if (null? l1)
        l2
        (reverse-iter (cdr l1) (cons (car l1) l2))))
  (reverse-iter items (list)))

(reverse (list 1 2 3 4))
(reverse (list 1 4 9 16 25))
