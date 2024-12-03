#lang sicp

; newtons-methodの手続きと一緒に
; (newtons-method (cubic a b c) 1)
; の形の式で使い, 三次式x3 + ax2 + bx + cの零点を近似する手続き cubicを定義せよ.

; --- 1.3.3 の fixed-point
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

; --- 1.3.4 の newtons-method

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (square x) (* x x))

(define (cubic a b c)
  (lambda (x) (+
               (cube x)
               (* a (square x))
               (* b x)
               c
               )))
               

(newtons-method (cubic 1 2 3) 1)
