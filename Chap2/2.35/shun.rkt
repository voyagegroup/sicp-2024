#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (count-leaves-old x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves-old (car x))
                 (count-leaves-old (cdr x))))))

(define (fringe tree)
  (if (pair? tree)
      (if (pair? (car tree))
          (append (fringe (car tree)) (fringe (cdr tree)))
          (cons (car tree) (fringe (cdr tree))))
      tree))

(define (count-leaves x)
  (accumulate + 0 (map (lambda (x) 1) (fringe x))))
; fringeでフラットにした1つのリストに対してmapで各要素に1を返す関数を適用して、それをaccumulateで足し合わせる

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)
; 4

(count-leaves (list x x))
; 8
