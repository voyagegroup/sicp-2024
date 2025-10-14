#lang sicp

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-in (make-wire)))
    (if (null? a-list)
        'ok
        (begin
          ; 最下位ビットの処理
          (full-adder (car a-list) 
                      (car b-list)
                      c-in
                      (car s-list)
                      c-out)
          ; 再帰的に残りのビットを処理
          (ripple-carry-adder (cdr a-list)
                              (cdr b-list)
                              (cdr s-list)
                              c-in))))
  ; 最初のc-inは桁上りがないので0
  (set-signal! c-in 0)
  'ok)

#|
半加算器の遅延
- 経路1: AND + not + AND
- 経路2: OR + AND
- 経路3: AND
(AND + not) > OR がtrueの場合は経路1が最長、falseの場合は経路2が最長

全加算器の遅延
- (半加算器の遅延 * 2) + OR

nビットの遅延
- n * (2 * 半加算器の遅延 + OR)
|#

