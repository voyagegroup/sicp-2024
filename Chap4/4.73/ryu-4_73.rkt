#lang sicp

; 本文
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

; 問題
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream))))) ; ここのdelayがきえている

; interleaveをする時に、 (stream-car) と (flatten-stream (stream-cdr)) も評価するようになる。

; stream = (S1, S2, S3, S4) の場合

; 本文の場合は、
; flatten-stream
; (interleave S1 (delay (flatten-stream (S2, S3, S4))
; delayで保留される

; が、もんだいのは
; flatten-stream
; (interleave S1 (flatten-stream (S2, S3, S4)
; (interleave S2 (flatten-stream (S3, S4)
; ...
; と全部深ぼっていくことになる。
; もし、streamが無限の場合、前の問題と同じように、なにも出力されずに無限ループに行く。


