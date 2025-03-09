#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


(define (make-s-by-trio n s)
  (filter (lambda (x) (= (+ (car x) (cadr x) (caddr x)) s)) (flatmap (lambda (i)
                   (flatmap (lambda (j)
                          (map (lambda (k) (list i j k))
                               (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))))

(make-s-by-trio 5 8)
; '((4 3 1) (5 2 1))