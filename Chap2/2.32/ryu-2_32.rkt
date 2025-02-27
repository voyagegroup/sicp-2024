#lang sicp

(define my-list (list 1 2 3))
my-list
; (1 2 3)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (l)
                        (display "----")(newline)
                        (display "s: ")
                        (display s)(newline)
                        (display "l: ")
                        (display l)(newline)
                        (display "(cons (car s) l):")
                        (display (cons (car s) l))(newline)
                        (cons (car s) l))
                      rest)))))

(subsets my-list)

#| --output--
----
s: (3)
l: ()
(cons (car s) l):(3)
----
s: (2 3)
l: ()
(cons (car s) l):(2)
----
s: (2 3)
l: (3)
(cons (car s) l):(2 3)
----
s: (1 2 3)
l: ()
(cons (car s) l):(1)
----
s: (1 2 3)
l: (3)
(cons (car s) l):(1 3)
----
s: (1 2 3)
l: (2)
(cons (car s) l):(1 2)
----
s: (1 2 3)
l: (2 3)
(cons (car s) l):(1 2 3)
(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
|#

; 3の部分集合 -> (), (3)
; 3の部分集合 + 2 -> (2), (2, 3)
; 3の部分集合 + 2と3の部分集合 + 1 -> (1), (1, 3), (1, 2), (1, 2, 3)