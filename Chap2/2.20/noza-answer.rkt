#lang racket

(define (same-parity x . w)
  (define (same-parity-iter l)
    (cond ((null? l) '())
          ((equal? (odd? x) (odd? (car l))) (cons (car l) (same-parity-iter (cdr l))))
          (else (same-parity-iter (cdr l)))))
  (cons x (same-parity-iter w)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

