#lang racket

(define (deep-reverse items)
  (if (pair? items)
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))
      items))

(deep-reverse (list (list 1 2) (list 3 4)))

