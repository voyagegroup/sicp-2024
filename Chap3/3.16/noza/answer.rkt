#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define list-three (list 'a 'b 'c))

(define list-a (cons 'a '()))
(define list-aa (cons list-a '()))
(define list-four (cons list-aa list-a))

(define list-b (cons 'b '()))
(define list-bb_bb (cons list-b list-b))
(define list-seven (cons list-bb_bb list-bb_bb))

(count-pairs list-three)
(count-pairs list-four)
(count-pairs list-seven)
