#lang racket

;; 本からコピー
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))
; ここまで

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((rect-area (* (- x2 x1) (- y2 y1)))
        (result (monte-carlo trials experiment)))
    (* rect-area result)))

(define (estimate-pi trials)
  (define (in-unit-circle? x y)
    (<= (+ (square x) (square y)) 1))
  (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 trials))

(define (square x) (* x x))

(display (format "  100 trials: ~a\n" (estimate-pi 100)))
(display (format " 1000 trials: ~a\n" (estimate-pi 1000)))
(display (format "10000 trials: ~a\n" (estimate-pi 10000)))
