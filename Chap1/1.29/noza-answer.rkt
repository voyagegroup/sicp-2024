#lang racket

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-iter i sum)
    (cond
      ((= i n) (+ sum (/ (* h (f (+ a (* i h)))) 3)))
      ((= i 0) (+ (* (/ h 3) (* h (f a))) (simpson-iter (+ i 1) sum)))
      ((= (remainder i 2) 0) (+ sum (* (/ h 3) (* 2 (f (+ a (* i h))))) (simpson-iter (+ i 1) sum)))
      (else (+ sum (* (/ h 3) (* 4 (f (+ a (* i h))))) (simpson-iter (+ i 1) sum)))))
  (simpson-iter 0 0))

(define (cube x) (* x x x))


(integral cube 0 1 0.01)  ; 0.24998750000000042
(integral cube 0 1 0.001) ; 0.249999875000001
(simpson cube 0 1 100)    ; 1/4
(simpson cube 0 1 1000)   ; 1/4

; より正確にできている
         
