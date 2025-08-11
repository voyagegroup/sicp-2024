#lang racket

(define random-init 42)

(define rand
  (let ((x random-init))
    (lambda (action)
      (cond ((eq? action 'generate)
             (set! x (rand-update x))
             x)
            ((eq? action 'reset)
             (lambda (new-value)
               (set! x new-value)
               'done))
            (else
             (error "Unknown action -- RAND" action))))))

(define (rand-update x)
  (let ((a 1664525)
        (b 1013904223)
        (m 4294967296))
    (modulo (+ (* a x) b) m)))

(rand 'generate)
(rand 'generate)
((rand 'reset) 42)
(rand 'generate)
(rand 'generate)
