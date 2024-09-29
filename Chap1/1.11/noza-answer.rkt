#lang racket

; 再帰的プロセス
(define (fib-ext n)
    (cond
        ((< n 4) n)
        (else (+ (fib-ext (- n 1)) (fib-ext (- n 2)) (fib-ext (- n 3))))))

; 手計算すると: 1 2 3 6 11 20 37 68 125 230
;(fib-ext 1)
;(fib-ext 2)
;(fib-ext 3)
;(fib-ext 4)
;(fib-ext 5)
;(fib-ext 6)
;(fib-ext 7)
;(fib-ext 8)
;(fib-ext 9)
;(fib-ext 10) 

; (fib-ext 6)
; (+ (fib-ext (- 6 1)) (fib-ext (- 6 2)) (fib-ext (- 6 3)))
; (+
;   (fib-ext 5)
;   (fib-ext 4)
;   (fib-ext 3))
; (+
;   (+
;     (fib-ext (- 5 1))
;     (fib-ext (- 5 2))
;     (fib-ext (- 5 3)))
;   (+ 
;     (fib-ext (- 4 1))
;     (fib-ext (- 4 2))
;     (fib-ext (- 4 3)))
;   (fib-ext 3))
; (+
;   (+
;     (fib-ext 4)
;     (fib-ext 3)
;     (fib-ext 2))
;   (+
;     (fib-ext 3)
;     (fib-ext 2)
;     (fib-ext 1))
;   3)
; (+
;   (+
;     (+ 
;       (fib-ext 3)
;       (fib-ext 2)
;       (fib-ext 1))
;     3
;     2)
;   (+ 
;     3
;     2
;     1)
;   3)
; (+
;  (+
;    (+ 3 2 1)
;    3
;    2)
;  (+ 3 2 1)
;  3)
;  20
;

; 反復的プロセス
(define (fib-ext-2 n)
    (define (fib-iter a b c count)
        (cond ((= count 0) a)
              ((= count 1) b)
              ((= count 2) c)
              (else (fib-iter b c (+ a b c) (- count 1)))))
    (fib-iter 1 2 3 (- n 1)))

; 手計算すると: 1 2 3 6 11 20 37 68 125 230
;(fib-ext-2 1)
;(fib-ext-2 2)
;(fib-ext-2 3)
;(fib-ext-2 4)
;(fib-ext-2 5)
;(fib-ext-2 6)
;(fib-ext-2 7)
;(fib-ext-2 8)
;(fib-ext-2 9)
;(fib-ext-2 10)

; (fib-ext-2 6)
; (fib-itr 1 2 3 5)
; (fib-itr 2 3 (+ 1 2 3) 4)
; (fib-iter 2 3 6 4)
; (fib-iter 3 6 (+ 2 3 6) 3)
; (fib-iter 6 11 (+ 3 6 11) 2)
; 20

