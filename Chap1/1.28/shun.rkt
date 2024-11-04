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

(define (fast-prime? n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;#f
(fast-prime? 561 1000)
(fast-prime? 1105 1000)
(fast-prime? 1729 1000)

(fast-prime? 2465 1000)
(fast-prime? 2821 1000)
(fast-prime? 6601 1000)

;#t
(fast-prime? 1009 1000)
(fast-prime? 1013 1000)
(fast-prime? 1019 1000)

(fast-prime? 10007 1000)
(fast-prime? 10009 1000)
(fast-prime? 10037 1000)

(fast-prime? 100003 1000)
(fast-prime? 100019 1000)
(fast-prime? 100043 1000)

(fast-prime? 1000003 1000)
(fast-prime? 1000033 1000)
(fast-prime? 1000037 1000)