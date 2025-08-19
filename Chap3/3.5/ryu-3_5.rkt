#lang sicp


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(random-in-range 25 30)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (50-percen)
  (< (random 2) 1)) 

(monte-carlo 1000 50-percen)
; 257/500
; だいたい0.5になっている

; 実装ここから

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials experiment) ; monte-carloの結果に四角形の面積をかける
     (* (- x2 x1) (- y2 y1))))

(define (unit-circle-test x y)
  (<= (+ (* x x) (* y y)) 1))


(define (estimate-pi-integral trials)
  (estimate-integral unit-circle-test -1.0 1.0 -1.0 1.0 trials))

(estimate-pi-integral 1000)
; 1回目: 3.184
; 2回目: 3.12
; 3回目: 3.16