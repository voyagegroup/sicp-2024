#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define list-three (list 'a 'b 'c))
(define list-four (cons '((a)) '(a)))
(define list-seven '(((a) a) (a) a))

(count-pairs list-three)
(count-pairs list-four)
(count-pairs list-seven)