#lang sicp

; (1)
(define one (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr one)))))

; (2)
(define two (list (list 7)))
(car (car two))

; (3)
(define three (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr three))))))))))))