#lang racket

(define (square n)
  (* n n))

(define (divides? x y)
  (= (remainder y x) 0))

(define (even? n)
  (= (remainder n 2) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

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

; 1000の時
; 1009 *** 0.001220703125
; 1013 *** 0.0009765625
; 1019 *** 0.001220703125

; 10000の時
; 10007 *** 0.003173828125
; 10009 *** 0.004150390625
; 10037 *** 0.0029296875

; 100000の時
; 100003 *** 0.010009765625
; 100019 *** 0.010009765625
; 100043 *** 0.009033203125

; 1000000の時
; 1000003 *** 0.029052734375
; 1000033 *** 0.030029296875
; 1000037 *** 0.029052734375

; ほぼ3倍(√10倍)になっていると言える。