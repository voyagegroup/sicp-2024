#lang sicp

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result-stream
            (qeval (unique-query operands)
                   (singleton-stream frame))))
       (if (and (not (stream-null? result-stream))
                (stream-null? (stream-cdr result-stream)))
           result-stream
           the-empty-stream)))
   frame-stream))

(define (unique-query operands)
  (car operands))

(define install-unique
  (put 'unique 'qeval uniquely-asserted))
