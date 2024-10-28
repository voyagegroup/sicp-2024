#lang racket

(define (square x) (* x x))

(define (runtime) (current-inexact-milliseconds))

; 本文のコピー
(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

; 問題
(define (next x)
  (if (= x 2) 3 (+ x 2)))

(define (smallest-divisor-ext n)
  (find-divisor-ext n 2 1))


; 問題1.22のコピー
(define (find-divisor-ext n test-divisor count)
  (display "count: ")
  (display count)
  (newline)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor-ext n (next test-divisor) (+ count 1)))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))
    0))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-prime st end)
  (if (> st end)
    (begin
     (display "end")
     (void))
    (begin
       (timed-prime-test st)
       (search-prime (+ st 2) end))))

; prime?の置き換え
(define (prime? n)
  (= n (smallest-divisor-ext n)))

;(search-prime 1001 1101)
;(search-prime 10001 10101)
;(search-prime 100001 100101)
;(search-prime 1000001 1000101)

;1009 *** 0.000244140625
;1013 *** 0.000244140625
;1019 *** 0.000244140625

;10007 *** 0.0009765625
;10009 *** 0.0009765625
;10037 *** 0.001220703125

;100003 *** 0.002197265625
;100019 *** 0.002197265625
;100043 *** 0.002685546875

;1000003 *** 0.0068359375
;1000033 *** 0.0068359375
;1000037 *** 0.007080078125

(search-prime 1000003 1000004)

