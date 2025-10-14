#lang sicp


; 3.28
(define (or-gate-3-28 a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logial-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)


; 3.29
(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (and-output (make-wire)))
    ; 入力を反転
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    ; 反転した入力のAND
    (and-gate not-a1 not-a2 and-output)
    ; 結果を反転して出力
    (inverter and-output output)
    'ok))


#|

遅延の計算
流れ
1. inverter-delay （a1とa2は同時）
2. and-gate-delay
3. inverter-delay

合計: (inverter-delay * 2) + and-gate-delay
|#