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

(define (n-root-times x n times)
  (fixed-point ((repeated average-damp times) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

#| (n-root 2 2 10) |#
#| (n-root 3 2 10) |#
#| (n-root 8 3 10) |#
#| (n-root 27 3 10) |#
#| (n-root 16 4 10) |#

(n-root-times 2 2 1)
(n-root-times 2 3 1)
(n-root-times 2 4 2)
(n-root-times 2 5 2)
(n-root-times 2 6 2)
(n-root-times 2 7 2)
(n-root-times 2 8 3)
(n-root-times 2 9 3)
(n-root-times 2 10 3)
(n-root-times 2 11 3)
(n-root-times 2 12 3)
(n-root-times 2 13 3)
(n-root-times 2 14 3)
(n-root-times 2 15 3)
(n-root-times 2 16 4)

(define (n-root x n)
  (let ((times (floor (/ (log x) (log n)))))
    (n-root-times x n times)))

(n-root 2 2)
(n-root 3 2)
(n-root 8 3)
(n-root 27 3)
(n-root 16 4)
