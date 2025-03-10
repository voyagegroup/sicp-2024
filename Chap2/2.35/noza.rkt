#lang racket

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (fringe items)
  (if (pair? items)
      (if (pair? (car items))
          (append (fringe (car items)) (fringe (cdr items)))
          (cons (car items) (fringe (cdr items))))
      items))

(define (count-leaves x)
 (accumulate + 0 (map (lambda (y) 1) (fringe x))))

(define x (list (list 1 2) (list 3 4)))

(count-leaves x)
