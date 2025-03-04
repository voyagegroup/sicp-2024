#lang sicp
; -- 2.36 からコピペ --
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (filter seqs (list)))
            (accumulate-n op init (filter-2 seqs (list))))))

(define (filter seqs sequences)
  (if (null? seqs)
      sequences
      (filter (cdr seqs) (append sequences (list (car (car seqs)))))))

(define (filter-2 old-seqs new-seqs)
  (if (null? old-seqs)
      new-seqs
      (filter-2 (cdr old-seqs) (append new-seqs (list (cdr (car old-seqs)))))))


; -- ここから --
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define v (list 1 2 3))
(define w (list 4 5 6))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v w)
; -> 32

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
m
; -> ((1 2 3 4) (4 5 6 6) (6 7 8 9))
(define v2 (list 1 2 3 4))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector m v2)
; -> (30 56 80)


(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose m)
; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define n (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n))) ; cols: ((1 4 7 10) (2 5 8 11) (3 6 9 12))
    (map (lambda (x) (matrix-*-vector cols x)) m))) ; x: (1 2 3 4)(4 5 6 6)(6 7 8 9)


(matrix-*-matrix m n)
; ((70 80 90) (126 147 168) (180 210 240))