#lang racket

(require amb)

; SICP の require に対応: p が偽なら amb で失敗
(define (require p)
  (unless p (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; 最初の一解だけ表示
(displayln (stream-first (in-amb (a-pythagorean-triple-between 1 20))))
