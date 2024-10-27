#lang racket

(define (runtime) (current-inexact-milliseconds))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; 問題にあった関数

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

(search-prime 1001 1101)
(search-prime 10001 10101)
(search-prime 100001 100101)

; 1009 *** 0.00048828125
; 1013 *** 0.00048828125
; 1019 *** 0.000732421875

;10007 *** 0.001220703125
;10009 *** 0.001708984375
; 10037 *** 0.00146484375

;100003 *** 0.00390625
;100019 *** 0.00390625
;100043 *** 0.003662109375


