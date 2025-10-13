#lang sicp

(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (and-not-a1-not-a2 (make-wire))
        (not-and-not-a1-not-a2 (make-wire)))
    (ivnerter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate not-a1 not-a2 and-not-a1-not-a2)
    (inverter and-not-a1-not-a2 output)
    'ok))

; 遅延は 2 * inverter-delay + and-gate-delay

; a1 0 0 1 1
; a2 0 1 0 1
; (inverter a1) 1 1 0 0
; (inverter a2) 1 0 1 0
; (and (inverter a1) (inverter a2)) 1 0 0 0
; (inverter (and (inverter a1) (inverter a2))) 0 1 1 1