#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

; --- ここから新規実装

(define (unique-trios n)
  (flatmap
   (lambda (i)
     (map (lambda (k) (append (list i) k))
          (unique-pairs (- i 1))))
   (enumerate-interval 1 n)))

; -- 3組をつくる
(unique-trios 6)
; ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3) (6 2 1) (6 3 1) (6 3 2) (6 4 1) (6 4 2) (6 4 3) (6 5 1) (6 5 2) (6 5 3) (6 5 4))

; -- 3つを足した値をlistの最後につける
(define (sum-trio trio)
  (+ (car trio) (cadr trio) (caddr trio)))
(define (make-trio-sum trio)
  (list (car trio) (cadr trio) (caddr trio) (sum-trio trio)))

(map make-trio-sum  (unique-trios 6))


(define (same-sum-s n s)
  (define (same-sum-s? trio) ; -- 合計がsと一致するかを確認する
    (= (sum-trio trio) s))
  
  (filter same-sum-s?
          (map make-trio-sum (unique-trios n))))

(same-sum-s 6 15)
; ((6 5 4 15))
(same-sum-s 6 10)
; ((5 3 2 10) (5 4 1 10) (6 3 1 10))
          
          
   
          