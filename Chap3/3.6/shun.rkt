#lang sicp

(define random-init 7)

(define rand
  (let ((x random-init))
    (lambda (query)
      (cond ((eq? query 'generate) (set! x (rand-update x)) x)
            ((eq? query 'reset (lambda (new-value) (set! x new-value) x)))))))