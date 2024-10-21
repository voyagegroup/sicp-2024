#lang racket

(define (square n)
  (* n n))

(define (divides? x y)
  (= (remainder y x) 0))

(define (even? n)
  (= (remainder n 2) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (runtime)
  (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (void)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (one-or-two n)
  (if (even? n)
      1
      2))

(define (search-for-primes start end)
  (cond ((< start 2) start)
        ((> start end) (void))
        (else (timed-prime-test start)
              (search-for-primes (+ start (one-or-two start)) end))))

(search-for-primes 1000 1100)

; 1009 *** 0.001953125
; 1013 *** 0.002197265625
; 1019 *** 0.0009765625

; 10007 *** 0.004150390625
; 10009 *** 0.0029296875
; 10037 *** 0.0029296875

; 100003 *** 0.009033203125
; 100019 *** 0.009033203125
; 100043 *** 0.009033203125

; 1000003 *** 0.028076171875
; 1000033 *** 0.028076171875
; 1000037 *** 0.027099609375

; 多少短くなっているものの2倍ほどの変化はない。