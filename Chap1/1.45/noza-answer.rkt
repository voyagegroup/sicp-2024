#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; 前の問題から持ってきたやつ
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated-iter (- n 1)))))
  (repeated-iter n))
; ここまで

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (n-root x n times)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) times) 1.0))

(n-root 16 4 10)
