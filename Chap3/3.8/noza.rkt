#lang racket

;; 内部状態を持ち、最初の呼び出しで引数を返し、2回目は0を返す
(define f
  (let ((state 'first))
    (lambda (x)
      (if (eq? state 'first)
          (begin
            (set! state 'second)
            x)
          0))))

;; 左から評価すると
;; (+ 0 (f 1))
;; (+ 0 0)
;; 0

;; 右から評価すると
;; (+ (f 0) 1)
;; (+ 0 1)
;; 1
