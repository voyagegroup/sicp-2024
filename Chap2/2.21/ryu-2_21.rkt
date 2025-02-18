#lang sicp

(define (square x)
  (* x x))

; 1つ目
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))
; -> (1 4 9 16)

; 2つ目
(define (square-list-map items)
  (map square items))

(square-list-map (list 1 2 3 4))
; -> (1 4 9 16)
