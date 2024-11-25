#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; 平均を使わない普通の場合
(define (sut)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 4))

(sut)
; 29ステップで、4.555539183677709が得られた

; 平均緩和法を使った場合
(define (sut-avg)
  (define (average x y)
  (/ (+ x y) 2))
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 4))

; 7ステップで、4.5555342036887705が得られた