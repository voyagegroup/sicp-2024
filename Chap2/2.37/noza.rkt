#lang racket

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define mt
 (list (list 1 2 3 4)
       (list 4 5 6 6)
       (list 6 7 8 9)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(matrix-*-vector mt (list 1 2 3 4))

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose mt)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (map (lambda (c) (dot-product r c)) cols)) m)))

(matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8)))
; '((19 22) (43 50))
; (1*5 + 2*7) = 19, (1*6 + 2*8) = 22
; (3*5 + 4*7) = 43, (3*6 + 4*8) = 50
