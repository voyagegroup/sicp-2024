#lang racket

(define (square x) (* x x))

(define (expmod base expp m)
  (cond 
    ((= expp 0) 1)
    ((even? expp)
     (if (and 
        (not (= base 1))
        (not (= base (- expp 1)))
        (= (remainder (square (remainder (square (expmod base (/ expp 2) m)) m)) m) 1)) 
      0 
      1))
    (else
      (remainder (* base (expmod base (- expp 1) m)) m))))

(define (fast-prime? n times)
  (cond 
    ((= times 0) true)
    ((miller-rabin n) (fast-prime? n (- times 1)))
    (else false)))

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-st-ed st ed)
  (if (< st ed)
    (begin
      (display st)
      (display ": ")
      (display (fast-prime? st 10000))
      (newline)
      (miller-rabin-st-ed (+ st 1) ed))
    (void)))

(miller-rabin-st-ed 2 103)
