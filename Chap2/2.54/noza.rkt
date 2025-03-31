#lang racket

(define (my-equal? a b)
  (or
    (and
      (not (pair? a))
      (not (pair? b))
      (eq? a b))
    (and
      (pair? a)
      (pair? b)
      (my-equal? (car a) (car b))
      (my-equal? (cdr a) (cdr b)))))

(my-equal? '(this is a list) '(this is a list))

(my-equal? '(this is a list) '(this (is a) list))

