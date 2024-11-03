#lang racket

(define (runtime) (current-inexact-milliseconds))

(define (square x) (* x x))

; 1.22の問題のコピー
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; 置き換えのためにコメントアウト
;(define (start-prime-test n start-time)
;  (if (prime? n)
;    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; 本文のコピー
(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
      (remainder (square (expmod base (/ exp 2) m)) m))
    (else
      (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond 
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

; 置き換え
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
    (report-prime (- (runtime) start-time))
    0))

(define (search-prime st end)
  (if (> st end)
    (begin
      (newline)
      (display "end")
      (void))
    (begin
      (timed-prime-test st)
      (search-prime (+ st 2) end))))

(search-prime 1001 1101)
(search-prime 10001 10101)
(search-prime 100001 100101)
(search-prime 1000001 1000101)


;1009 *** 0.43017578125
;1013 *** 0.484375
;1019 *** 0.4677734375
;
;10007 *** 0.538818359375
;10009 *** 0.507568359375
;10037 *** 0.522705078125
;
;100003 *** 0.60302734375
;100019 *** 0.82666015625
;100043 *** 0.73120117187
;
;
;1000003 *** 0.814208984375
;1000033 *** 0.72607421875
;1000037 *** 0.763427734375

; オーダーで考えるのであれば約3倍くらいになるはずだがそうはなっていない
; fast-prime? を 1000 と 10000 について 1 回で論理モデルの置き換えやってみる
