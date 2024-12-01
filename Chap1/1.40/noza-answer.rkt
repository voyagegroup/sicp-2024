#lang racket

; 本から持ってくる
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

(define dx 0.00001)

(define (deriv g)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x))
        dx)))

(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) x)))))


(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))
; ここまで

; cubic関数の実装
(define (cubic a b c)
    (lambda (x)
        (+
            (expt x 3)
            (* a (expt x 2))
            (* b x)
            c)))

(newtons-method (cubic 0 0 -8) 1.0)
; x^3 - 8 = 0 x -> 2.000000000036784
; 検算足りない気もするがいったんヨシッ！
