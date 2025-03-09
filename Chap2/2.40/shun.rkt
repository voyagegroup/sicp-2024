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


(define (unique-pairs n)
  (filter (lambda (li) (> (car li) (cadr li))) (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 n))) (enumerate-interval 1 n))))

(unique-pairs 3)
; '((2 1) (3 1) (3 2))
; '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))からi > jのペアを抽出する

