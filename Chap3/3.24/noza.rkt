#lang sicp

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc-entry key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc-entry key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-entry key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-entry key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-entry key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-entry key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; 以下動作検証
(define (make-numeric-same-key? tolerance)
  (lambda (a b)
    (<= (abs (- a b)) tolerance)))

(define (table-lookup-proc table)
  (table 'lookup-proc))

(define (table-insert-proc! table)
  (table 'insert-proc!))

(define (assert-equal expected actual label)
  (if (equal? expected actual)
      'ok
      (error "Test failed" label expected actual)))

(define (assert-false actual label)
  (if (eq? actual false)
      'ok
      (error "Test failed" label actual)))

(define (run-table-tests)
  (let* ((tolerance 0.1)
         (numeric-table (make-table (make-numeric-same-key? tolerance)))
         (numeric-lookup (table-lookup-proc numeric-table))
         (numeric-insert! (table-insert-proc! numeric-table)))
    (numeric-insert! 1.0 10.0 'voltage)
    (assert-equal 'voltage (numeric-lookup 1.05 10.03)
                  "numeric lookup within tolerance")
    (assert-false (numeric-lookup 1.3 10.0)
                  "numeric lookup outside tolerance")
    (numeric-insert! 0.96 9.97 'voltage-updated)
    (assert-equal 'voltage-updated (numeric-lookup 1.02 10.02)
                  "numeric insert updates existing entry")
    (let* ((exact-table (make-table equal?))
           (exact-lookup (table-lookup-proc exact-table))
           (exact-insert! (table-insert-proc! exact-table)))
      (exact-insert! 10 20 'exact-value)
      (assert-equal 'exact-value (exact-lookup 10 20)
                    "exact lookup matches identical keys")
      (assert-false (exact-lookup 10.15 20)
                    "exact lookup rejects different primary key"))
    'all-tests-passed))
