#lang sicp

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))


(define (my-for-each proc items)
    (cond ((null? items) nil)
          (else
           (proc (car items))
           (my-for-each proc (cdr items)))))


(my-for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; 57
; 321
; 88()

; memo: ()が消せない