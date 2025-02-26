#lang racket

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3))

;; (subsets '(1 2 3))
;; (append (subsets '(2 3)) (map (lambda (x) (cons 1 x)) (subsets '(2 3))))

;; subsets '(2 3)
;; (append (subsets '(3)) (map (lambda (x) (cons 2 x)) (subsets '(3))))

;; subsets '(3)
;; (append (subsets '()) (map (lambda (x) (cons 3 x)) (subsets '())))

;; subsets '()
;; ()

;; subsets '(3)
;; (append '() (map (lambda (x) (cons 3 x)) ()))
;; (append () (cons 3 ()))
;; (list () (list 3))
;; (() (3))

;; subsets '(2 3)
;; (append (() (3)) (map (lambda (x) (cons 2 x)) (() (3))))
;; (append (() (3)) (list (list 2) (list 2 3)))
;; (() (3) (2) (2 3))

;; subsets '(1 2 3)
;; (append (() (3) (2) (2 3)) (map (lambda (x) (cons 1 x)) (() (3) (2) (2 3))))
;; (append (() (3) (2) (2 3)) (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


