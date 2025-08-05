#lang sicp

; --sqrt
(define (square x) (* x x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
; --


(define (make-monitored f)
  (let ((count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1)) (f m)))))
    dispatch))

(define s (make-monitored sqrt))

(s 100)
; 10.000000000139897
(s 'how-many-calls?)
; 1

(s 16)
; 4.000000636692939
(s 'how-many-calls?)
; 2

(s 'reset-count)
(s 'how-many-calls?)
; 0
