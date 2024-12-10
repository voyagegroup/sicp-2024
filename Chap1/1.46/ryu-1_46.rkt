#lang sicp

; --- sqrt

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
; -> 3.00009155413138

; --- fixed-point

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

(fixed-point cos 1.0)

; ---- 本題

; ((iterative-improve good-enough? improve) guess)

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (first-guess) (iter first-guess)))


(define (sqrt-iter-improve x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))

(sqrt-iter-improve 4.0)
; -> 2.0000000929222947

(sqrt-iter-improve 2.0)
; -> 1.4142156862745097

(define (fixed-point-iter-improve f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance)) ; v1 -> guess, v2 -> (f guess)
  (define (improve guess)
    (f guess))
  ((iterative-improve close-enough? improve) first-guess))



(fixed-point-iter-improve cos 1.0)
; -> 0.7390893414033927