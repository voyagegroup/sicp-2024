;
#lang sicp

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
(factorial 6)
; (if (= 6 1)
;     1
;     (* 6 (factorial (- 6 1))))
; (if #f
;     1
;     (* 6 (factorial 5)))
; (* 6 (factorial 5))
; (* 6 (* 5 (factorial 4)))
; (* 6 (* 5 (* 4 (factorial 3))))
; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
; (* 6 (* 5 (* 4 (* 3 2))))
; (* 6 (* 5 (* 4 6)))
; (* 6 (* 5 24))
; (* 6 120)
; 720

(define (new-factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(new-factorial 6)
; (fact-iter 1 1 6)
; (if (> 1 6)
;     1
;     (fact-iter (* 1 1) (+ 1 1) 6))
; (if #f
;     1
;     (fact-iter 1 2 6))
; (fact-iter 1 2 6)
; (fact-iter 2 3 6)
; (fact-iter 6 4 6)
; (fact-iter 24 5 6)
; (fact-iter 120 6 6)
; (fact-iter 720 7 6)
; 720

;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
;(+ 4 5)
;(if (= 4 0)
;    5
;    (inc (+ (dec 4) 5)))
;(if #f
;    5
;    (inc (+ 3 5)))
;(inc (+ 3 5)))
;(inc (if (= 3 0)
;         5
;         (inc (+ (dec 3) 5))))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))
;(+ 4 5)
;(if (= 4 0)
;    5
;    (+ (dec 4) (inc 5)))
;(if #f
;    5
;    (+ 3 6))
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9

