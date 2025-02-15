#lang racket

(define (deep-reverse items)
  (define (iter items reversed)
    (if (null? items)
        reversed
        (if (pair? (car items))
            (iter (cdr items) (cons (deep-reverse (car items)) reversed))
            (iter (cdr items) (cons (car items) reversed)))
        ))
  (iter items (list)))

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)