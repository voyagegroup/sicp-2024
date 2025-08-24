#lang sicp

(define (rand-update x)
  (remainder (+ (* 1234 x) 567) 89))

(define rand
  (let ((x 1))
    (define (dispatch message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))
            (else (error "Unknown request -- RAND" message))))
    dispatch))


(rand 'generate)
; 21
(rand 'generate)
; 48
(rand 'generate)
; 80
((rand 'reset) 100)
(rand 'generate)
; 79
(rand 'generate)
; 64
((rand 'reset) 100)
(rand 'generate)
; 79
(rand 'generate)
; 64
((rand 'reset) 1)
(rand 'generate)
; 21