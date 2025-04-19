#lang racket

(define (make-tree entry left right)
  (list entry left right))

(define (key record)
  (car record))

(define (entry records)
  (car records))

(define (left-records records)
  (cadr records))

(define (right-records records)
  (caddr records))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
         (car set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-records set-of-records)))
        (else (lookup given-key (right-records set-of-records)))))

(define tree
  (make-tree (list 7 "a") 
    (make-tree (list 3 "b") 
      (make-tree (list 1 "c") '() '()) 
      (make-tree (list 5 "d") '() '())) 
    (make-tree (list 9 "e") 
      '() 
      (make-tree (list 11 "f") '() '()))))

(lookup 3 tree)
; '(3 "b")

(lookup 4 tree)
; #f