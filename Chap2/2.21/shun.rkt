#lang racket

(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list (cdr items)))))


(square-list (list 1 2 3 4))
; '(1 4 9 16)

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))


(define (square-list-v2 items)
  (map (lambda (n) (* n n)) items))

(square-list-v2 (list 1 2 3 4))
; '(1 4 9 16)
