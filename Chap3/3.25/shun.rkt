#lang sicp

(define (make-table)
  (list '*table*))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup keys table)
  (define (iter ks holder)
    (let ((key-1 (car ks))
          (key-2s (cdr ks)))
      (let ((record (assoc key-1 (cdr holder))))
        (if record
            (if (null? key-2s)
                (cdr record)           ; 見つかったとき値を返す
                (iter key-2s record))
            false))))
  (if (null? keys)
      (error "lookup: keys must be non-empty")
      (iter keys table)))

(define (insert! keys value table)
  (define (insert-itr ks value holder)
    (let ((key-1 (car ks))
          (key-2s (cdr ks)))
      (let ((record  (assoc (car ks) (cdr holder)))
            (records (cdr holder)))
        (if (null? key-2s)
            (if record
                (set-cdr! record value)
                (set-cdr! holder (cons (cons key-1 value) records)))
            (let ((new-record
                   (if record
                       record
                       (begin
                         ;; (key-1 . '()) を先頭に追加し、そのレコードを次の holder にする
                         (set-cdr! holder (cons (cons key-1 '()) records))
                         (car (cdr holder))))))
              (insert-itr key-2s value new-record))))))
  (if (null? keys)
      (error "insert!: keys must be non-empty")
      (insert-itr keys value table))
  'ok)

(define t1 (make-table))

(insert! (list 'math +) 43 t1)
(insert! (list 'math -) 45 t1)
(insert! (list 'math *) 42 t1)
(insert! (list 'letters 'a) 97 t1)
(insert! (list 'letters 'b) 98 t1)

(lookup (list 'math +) t1)
; 43
(lookup (list 'letters 'a) t1)
; 97
(lookup (list 'letters 'z) t1)
; false