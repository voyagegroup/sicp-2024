#lang racket

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((and (and (not (= base 1)) (not (= base (- m 1)))) (= 1 (remainder (square base) m))) 0)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (mr-test n)
  (define (test-a a)
    (if (>= a n)
        true
        (if (= (expmod a n n) a)
            (test-a (+ a 1))
            false)))
  (test-a 1))

;#f
(mr-test 561)
(mr-test 1105)
(mr-test 1729)

(mr-test 2465)
(mr-test 2821)
(mr-test 6601)

;#t
(mr-test 1009)
(mr-test 1013)
(mr-test 1019)

(mr-test 10007)
(mr-test 10009)
(mr-test 10037)

(mr-test 100003)
(mr-test 100019)
(mr-test 100043)

(mr-test 1000003)
(mr-test 1000033)
(mr-test 1000037)