#lang sicp

(define (reverse items)
  (define (iter items reversed)
    (if (null? items)
        reversed
        (iter (cdr items) (cons (car items) reversed))))
  (iter items (list)))
        
(reverse (list 1 4 9 16 25))
; (25 16 9 4 1)

(reverse (list 25 1))
; (1 25)

(append (list 1 2) (list 5))
