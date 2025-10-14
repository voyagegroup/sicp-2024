#lang sicp

; -- and

(define (get-signal wire)
  (wire 'get-signal))

(define (logical-and s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signal" s1 s2))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; -- or
(define (logical-or s1 s2)
;  (cond ((and (= s1 0) (= s2 0)) 0)
;        ((and (= s1 1) (= s2 0)) 1)
;        ((and (= s1 0) (= s2 1)) 1)
;        ((and (= s1 1) (= s2 1)) 1)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((or (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logial-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


