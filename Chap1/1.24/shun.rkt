#lang racket

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (runtime)
  (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      (void)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)


;1009 *** 0.01318359375
;1013 *** 0.010986328125
;1019 *** 0.01220703125

;10007 *** 0.01318359375
;10009 *** 0.010986328125
;10037 *** 0.01220703125

;100003 *** 0.016845703125
;100019 *** 0.014892578125
;100043 *** 0.01416015625

;1000003 *** 0.015869140625
;1000033 *** 0.01611328125
;1000037 *** 0.014892578125

;1000近くと1000000近くでは、log1000なので約3倍速くなっているはずであるが、違いはほとんどない。
;正直違いが出ない理由がわからない。