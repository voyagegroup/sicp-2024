#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product '(1 2 3) '(4 5 6))
; 32

; 結果ベクトルtの各要素tiは、行列mのi行目の各要素と、ベクトルvの対応する要素との積の総和
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(matrix-*-vector s '(4 3 2 1))
; '(20 49 70)

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose '((7 8) (9 10) (11 12)))
; '((7 9 11) (8 10 12))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

; 結果行列pの(i,j)要素は、第1の行列mの第i行と第2の行列nの第j列の対応する要素同士の積の総和
(matrix-*-matrix '((1 2 3) (4 5 6)) '((7 8) (9 10) (11 12)))
; '((58 64) (139 154))
; (1*7 + 2*9 + 3*11) = 58,  (1*8 + 2*10 + 3*12) = 64
; (4*7 + 5*9 + 6*11) = 139, (4*8 + 5*10 + 6*12) = 154
