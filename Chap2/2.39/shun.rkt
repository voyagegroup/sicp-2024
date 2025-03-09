#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(reverse-right '(1 2 3))
; (append (fold-right op '() '(2 3)) '(1))
; '(3 2 1)

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse-left '(1 2 3))
; (cons (reverse-left op `() '(2 3)) 1)
;'(3 2 1)