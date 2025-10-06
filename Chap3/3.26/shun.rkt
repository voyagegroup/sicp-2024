#lang sicp

(define (make-table)
  (list (cons 9999 '()) '() '()))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (entry records)
  (car records))

(define (value record)
  (cdr record))

(define (left-records records)
  (cadr records))

(define (right-records records)
  (caddr records))

(define (lookup key table)
   (cond ((= (car (entry table)) key) (value (entry table)))
            ((< key (car (entry table))) (lookup key (left-records table)))
            ((> key (car (entry table))) (lookup key (right-records table)))
            (else false)))


(define (insert! key value table)
  (cond ((= key (car (entry table))) (set-cdr! (entry table) value))
        ((< key (car (entry table)))
         (if (null? (left-records table))
             (set-car! (cdr table) (list (cons key value) '() '()))
             (insert! key value (left-records table))))
        ((> key (car (entry table)))
         (if (null? (right-records table))
             (set-car! (cddr table) (list (cons key value) '() '()))
             (insert! key value (right-records table))))
        (else (error key value table)))
  'ok)

(define t1 (make-table))

(insert! 7 'a t1)

(insert! 5 'b t1)

(insert! 9 'c t1)

(lookup 7 t1)
; a

(lookup 5 t1)
; b

(lookup 9 t1)
; c