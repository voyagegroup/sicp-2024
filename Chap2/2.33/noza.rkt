#lang racket

(define l1 (list 1 2 3 4 5))
(define l2 (list 6 7 8 9 10))

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (map p sequence)
 (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (square n)
 (* n n))

(map square l1)

(define (append seq1 seq2)
 (accumulate cons seq2 seq1))

(append l1 l2)

(define (length sequence)
 (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length l1)
