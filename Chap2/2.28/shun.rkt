#lang racket

(define (fringe tree)
    (define (iter items result)
      (if (null? items)
          result
          (if (pair? (car items))
              (iter (cdr items) (append result (fringe (car items))))
              (iter (cdr items) (append result (list (car items)))))))
  (iter tree '()))

(define x (list (list 1 (list 2)) (list 3 4)))

(fringe x) 
;'(1 2 3 4)

(fringe (list x x))
;'(1 2 3 4 1 2 3 4)