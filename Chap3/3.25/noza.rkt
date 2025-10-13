#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    ;; ノードは (key value . children) という構造で保持する。
    (define (make-node key value children)
      (cons key (cons value children)))

    (define (node-value node)
      (cadr node))

    (define (set-node-value! node new-value)
      (set-car! (cdr node) new-value))

    (define (node-children node)
      (cddr node))

    ; 子ノードを持つセルについて key を持つ要素があればそれを返し、そうでなければ新しく作成して返す
    (define (ensure-node! parent-cell key)
      (let* ((nodes (cdr parent-cell))
             (existing (assoc key nodes)))
        (if existing
            existing
            (let ((new-node (make-node key false '())))
              (set-cdr! parent-cell (cons new-node nodes))
              new-node))))

    (define (lookup keys)
      (if (null? keys)
          (error "LOOKUP REQUIRES AT LEAST ONE KEY"))
      (define (iter nodes remaining-keys)
        (let* ((current-key (car remaining-keys))
               (node (assoc current-key nodes)))
          (if node
              (let ((rest (cdr remaining-keys)))
                (if (null? rest)
                    (let ((value (node-value node)))
                      (if value value false))
                    (let ((children (node-children node)))
                      (if (null? children)
                          false
                          (iter children rest)))))
              false)))
      (iter (cdr local-table) keys))

    (define (insert! keys value)
      (if (null? keys)
          (error "INSERT! REQUIRES AT LEAST ONE KEY"))
      (define (insert-iter parent-cell remaining-keys)
        (let* ((current-key (car remaining-keys))
               (node (ensure-node! parent-cell current-key))
               (rest (cdr remaining-keys)))
          (if (null? rest)
              (begin
                (set-node-value! node value)
                'ok)
              (insert-iter (cdr node) rest))))
      (insert-iter local-table keys))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (check-equal expected actual label)
  (if (equal? expected actual)
      'ok
      (error "TEST FAILED" label expected actual)))

(define (test-single-key-lookup)
  (let* ((table (make-table))
         (lookup (table 'lookup-proc))
         (insert! (table 'insert-proc!)))
    (insert! '(a) 1)
    (check-equal 1 (lookup '(a)) "single key lookup")))

(define (test-deep-key-lookup)
  (let* ((table (make-table))
         (lookup (table 'lookup-proc))
         (insert! (table 'insert-proc!)))
    (insert! '(a b c) 42)
    (check-equal 42 (lookup '(a b c)) "deep key lookup")
    (check-equal false (lookup '(a b)) "intermediate path is absent")))

(define (test-shared-prefix)
  (let* ((table (make-table))
         (lookup (table 'lookup-proc))
         (insert! (table 'insert-proc!)))
    (insert! '(b) 5)
    (insert! '(b c) 8)
    (check-equal 5 (lookup '(b)) "shared prefix root value")
    (check-equal 8 (lookup '(b c)) "shared prefix branch value")))

(define (test-value-overwrite)
  (let* ((table (make-table))
         (lookup (table 'lookup-proc))
         (insert! (table 'insert-proc!)))
    (insert! '(a) 1)
    (insert! '(a) 99)
    (check-equal 99 (lookup '(a)) "value overwrite")))

(define (test-missing-keys)
  (let* ((table (make-table))
         (lookup (table 'lookup-proc))
         (insert! (table 'insert-proc!)))
    (insert! '(a b c) 1)
    (check-equal false (lookup '(missing)) "lookup missing top-level")
    (check-equal false (lookup '(a b d)) "lookup missing deep key")))

(define (run-tests)
  (test-single-key-lookup)
  (test-deep-key-lookup)
  (test-shared-prefix)
  (test-value-overwrite)
  (test-missing-keys)
  '3.25-tests-passed)

(run-tests)
