#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f x)
  (if (= x 1)
      f
      (compose f (repeated f (- x 1)))))


(define (average a b c) (/ (+ (+ a b) c) 3.0))

(average 1 5 6)
; -> 4.0

; --- smooth ---

(define (square x) (* x x))


(define (smooth f dx)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

((smooth square 1) 5)
; -> 25.666666666666668
(average (square 4) (square 5) (square 6))
; -> 25.666666666666668
; smoothの動きは良さそうにみえる。

; ただ、これだと、smoothの引数が2つあるからrepeatedを使えない。
; dxを定数にするのがよいか

; ---- (define dx 1) を使うsmooth-v2 ----
(define dx 1)
(define (smooth-v2 f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

((smooth-v2 square) 5)
; -> 25.666666666666668

(define (n-fold-smooth f n)
  ((repeated  smooth-v2 n) f))

((n-fold-smooth square 1) 5)
; -> 25.666666666666668

11

((n-fold-smooth square 2) 5)
; (n-fold-smooth square 2)
; ((repeated smooth-v2 2) square)
; ((compose smooth-v2 (repeated smooth-v2 1)) square)
; ((compose smooth-v2 smooth-v2) square)
; (lambda (x) (smooth-v2 (smooth-v2 x)))
; (smooth-v2 (smooth-v2 square))
; ---
; ((smooth-v2 (smooth-v2 square)) 5)
((smooth-v2 (lambda (x) (average (square (- x 1)) (square x) (square (+ x 1))))) 5)
; 簡略化すると ((smooth-v2 xxx) 5)
; (average (xxx 4) (xxx 5) (xxx 6))
; (xxx 4) -> ((lambda (x) (average (square (- x 1)) (square x) (square (+ x 1)))) 4) -> (average (square 3) (square 4) (square 5))
; (xxx 5) -> ((lambda (x) (average (square (- x 1)) (square x) (square (+ x 1)))) 5) -> (average (square 4) (square 5) (square 6))
; (xxx 6) -> ((lambda (x) (average (square (- x 1)) (square x) (square (+ x 1)))) 6) -> (average (square 5) (square 6) (square 7))
; (average (average (square 3) (square 4) (square 5)) (average (square 4) (square 5) (square 6))  (average (square 5) (square 6) (square 7)))
; -> 26.333333333333332



((n-fold-smooth square 10) 5)
; -> 31.666666666666668
