#lang racket

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (sets) (cons (car s) sets)) rest)))))

(subsets (list 1 2 3))

; rest = (subsets (list 2 3))
; (append (subsets (list 2 3)) (map (lambda (sets) (cons (car (list 1 2 3)) sets))) (subsets (list 2 3))))
;   rest = (subsets (list 3))
;   (append (subsets (list 3)) (map (lambda (sets) (cons (car (list 2 3)) sets))) (subsets (list 3))))
;     rest = (subsets (list nil)) = (list nil)
;     (append (list nil) (map (lambda (sets) (cons (car (list 3)) sets))) (list nil)))
;     rest = (subsets (list 3)) = (list '() (list 3))
;   (append ('() (list 3)) (map (lambda (sets) (cons (car (list 2 3)) sets))) ('() (list 3))))
;   (append ('() (list 3)) (map (lambda (sets) (cons (list 2) sets))) ('() (list 3))))
;   (map (lambda (sets) (cons (list 2) ('() (list 3)))))  = (list (list 2) (list 2 3))
; rest = (subsets (list 2 3)) = ('() (list 3) (list 2) (list 2 3))
; (append ('() (list 3) (list 2) (list 2 3)) (map (lambda (sets) (cons (list 1) sets))) ('() (list 3) (list 2) (list 2 3)))
; (append ('() (list 3) (list 2) (list 2 3)) (map (lambda (sets) (cons (list 1) sets))) ('() (list 3) (list 2) (list 2 3)))
; (append ('() (list 3) (list 2) (list 2 3)) (list (list 1) (list 1 3) (list 1 2) (list 1 2 3)))
; (list '() (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3))
; このようにして、(list 1 2 3)の部分集合を求めることができる。