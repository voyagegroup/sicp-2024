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

(define (check-equal expected actual label)
  (if (equal? expected actual)
      'ok
      (error "TEST FAILED" label expected actual)))

(define (check-false actual label)
  (if (eq? actual false)
      'ok
      (error "TEST FAILED" label actual)))

(define (test-numeric-lookup-within-tolerance)
  (let* ((tolerance 0.1)
         (table (make-table (make-numeric-same-key? tolerance)))
         (lookup (table-lookup-proc table))
         (insert! (table-insert-proc! table)))
    (insert! 1.0 10.0 'voltage)
    (check-equal 'voltage (lookup 1.05 10.03)
                 "numeric lookup within tolerance")))

(define (test-numeric-lookup-outside-tolerance)
  (let* ((tolerance 0.1)
         (table (make-table (make-numeric-same-key? tolerance)))
         (lookup (table-lookup-proc table))
         (insert! (table-insert-proc! table)))
    (insert! 1.0 10.0 'voltage)
    (check-false (lookup 1.3 10.0)
                 "numeric lookup outside tolerance")))

(define (test-numeric-insert-updates-existing-entry)
  (let* ((tolerance 0.1)
         (table (make-table (make-numeric-same-key? tolerance)))
         (lookup (table-lookup-proc table))
         (insert! (table-insert-proc! table)))
    (insert! 1.0 10.0 'voltage)
    (insert! 0.96 9.97 'voltage-updated)
    (check-equal 'voltage-updated (lookup 1.02 10.02)
                 "numeric insert updates existing entry")))

(define (test-exact-lookup-matches-identical-keys)
  (let* ((table (make-table equal?))
         (lookup (table-lookup-proc table))
         (insert! (table-insert-proc! table)))
    (insert! 10 20 'exact-value)
    (check-equal 'exact-value (lookup 10 20)
                 "exact lookup matches identical keys")))

(define (test-exact-lookup-rejects-different-primary-key)
  (let* ((table (make-table equal?))
         (lookup (table-lookup-proc table))
         (insert! (table-insert-proc! table)))
    (insert! 10 20 'exact-value)
    (check-false (lookup 10.15 20)
                 "exact lookup rejects different primary key")))

(define (run-tests)
  (test-numeric-lookup-within-tolerance)
  (test-numeric-lookup-outside-tolerance)
  (test-numeric-insert-updates-existing-entry)
  (test-exact-lookup-matches-identical-keys)
  (test-exact-lookup-rejects-different-primary-key)
  '3.24-tests-passed)

(run-tests)
