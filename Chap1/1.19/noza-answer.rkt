#lang racket

(define (fib-iter a b p q count)
  (cond
    ((= count 0) b)
    ((even? count)
      (fib-iter a
        b
        (+ (* p p) (* q q))    ; p'を計算
        (+ (* 2 p q) (* q q))      ; q'を計算
        (/ count 2)))
    (else 
      (fib-iter 
        (+ (* b q) (* a q) (* a p))
        (+ (* b p) (* a q))
        p
        q
        (- count 1)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

; フィボナッチ数列: 0, 1, 1, 2, 3, 5, 8, 13, 21
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)

