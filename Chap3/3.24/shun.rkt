#lang sicp

(define (make-table same-key?)
  (define table (list '*table*))
  (define (lookup key)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))


(define (assoc key records)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
  (define (dispatch m)
    (cond ((eq? m 'lookup) lookup)
          ((eq? m 'insert!) insert!)
          (else (error m))))
  dispatch)

(define t1 (make-table equal?))

((t1 'insert!) 'a 1)

((t1 'lookup) 'a)
; 1


(define (pm1? n1 n2)
  (< (abs (- n2 n1)) 2))

(define t2 (make-table pm1?))

((t2 'insert!) 1 5)

((t2 'lookup) 0)
; 5

((t2 'lookup) 1)
; 5

((t2 'lookup) 2)
; 5

((t2 'lookup) 3)
; #f
