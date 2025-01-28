#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

; 中央値をだす
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

; 幅をだす
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define i (make-center-width 3.5 0.15))
i
; (3.35 . 3.65)

(center i) ; 3.5
(width i) ; 0.1499999999999999


; --- 解答
(define (make-center-percent c t)
  (make-interval (- c (* c t)) (+ c (* c t))))

(define (percent i)
  (/ (width i) (center i)))

(define i1 (make-center-percent 10 0.1))
; (make-interval (- 10 (* 10 0.1)) (+ 10 (* 10 0.1))))
; (make-interval (- 10 1.0) (+ 10 1.0))
; (make-interval 9.0 11.0)

i1
; (9.0 . 11.0)

(center i1)
; (/ (+ 11.0 9.0) 2)
; -> 10.0
(width i1)
; (/ (- 11.0 9.0) 2)
; -> 1.0

(percent i1)
; (/ (width i1) (center i1))
; (/ 1.0 10.0)
; -> 0.1
