#lang sicp

(define (require p)
  (if (not p) (amb)))

; 二つの与えられた限界の間の整数を返す手続き
(define (an-integer-between s e)
  (require (<= s e))
  (amb s (an-integer-between (+ s 1) e)))
  

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(a-pythagorean-triple-between 1 10)
; (3 4 5)
(amb)
; (6 8 10)
