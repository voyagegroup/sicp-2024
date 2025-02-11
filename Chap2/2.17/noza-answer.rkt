#lang racket

(define (last-pair l)
  (define (list-ref items)
    (if (null? (cdr items))
		items
		(list-ref (cdr items))))
  (list-ref l))

(last-pair (list 1 2 3 4))
(last-pair (list 23 72 149 34))