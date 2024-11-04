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

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (carmichael-test n)
  (define (test-a a)
    (if (>= a n)
        true
        (if (fermat-test n a)
            (test-a (+ a 1))
            false)))
  (test-a 1))

(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)

(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)

;#t
;#t
;#t
;#t
;#t
;#t
;全てtrueが返ってきてしまった