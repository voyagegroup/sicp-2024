#lang sicp

(define (make-monitored fn)
  (let ((count 0))
    (lambda (m)
      (cond
        ((eq? m 'how-many-calls?) count)
        ((eq? m 'reset-count) (set! count 0))
        (else (begin (set! count (+ count 1)) (fn m))))))
  )

(define s (make-monitored sqrt))

(s 100)
; 10

(s 100)
; 10

(s 'how-many-calls?)
; 2

(s 'reset-count)

(s 'how-many-calls?)
; 0