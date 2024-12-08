#lang racket

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (square n)
  (* n n))

(define (inc n)
  (+ n 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

(define (smooth-n f n)
   ((repeated smooth n) f))

((smooth-n square 2) 5)

; 25.00000000013333

; ((smooth-n square 2) 5)
; ((repeated smooth 2) square 5)
; ((lambda (x) (if (= 2 1) (square x) ((compose smooth (repeated smooth (- 2 1))) x))) 5)
; ((if (= 2 1) (square 5) ((compose smooth (repeated smooth (- 2 1))) 5)))
; ((compose smooth (repeated smooth (- 2 1))) 5)
; ((compose smooth (repeated smooth 1)) 5)
; ((compose smooth (lambda (x) (square x))) 5)
; ((lambda (y) (smooth ((lambda (x) (square x)) y))) 5)
; (smooth ((lambda (x) (square x)) 5))
; (smooth (square 5))
; (smooth 25)
; (/ (+ (25 -0.00001) 25 (25 0.00001)) 3)
; (/ (+ 24.99999 25 25.00001) 3)
; (/ 75.00001 3)
; 25.00000000013333


