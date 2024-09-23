;

#lang sicp

(define (sum-of-squares-of-largest-two a b c)
  (define (square x) (* x x)) ; 平方
  (cond ((and (<= a b) (<= a c)) (+ (square b) (square c))) ; aが最小
        ((and (<= b a) (<= b c)) (+ (square a) (square c))) ; bが最小
        (else (+ (square a) (square b)))))                  ; cが最小

(sum-of-squares-of-largest-two 1 2 3)
;13

(sum-of-squares-of-largest-two 1 4 3)
;25

(sum-of-squares-of-largest-two 5 4 3)
;41
;
