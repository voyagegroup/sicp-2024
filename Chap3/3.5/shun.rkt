#lang sicp

(define (square x) (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-integral p x1 x2 y1 y2 trial)
  (define (in-area-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (let ((cube (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo trial in-area-test) cube)
  ))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(estimate-integral 
  (lambda (x y) (<= (+ (square (- x 5)) (square (- y 7))) 9))
  2.0 8.0 4.0 10.0 1000)
; 28.728

; 28.728 / (* 3 3) // 半径の2乗で割る
; pi = 3.192