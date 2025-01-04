#lang racket
; 面積と周囲の求め方は共通で使う
(define (perimeter r)
  (+ (* (width r) 2) (* (height r) 2)))

(define (area r)
  (* (width r) (height r)))

; 対角を使って表現する場合
(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rectangle corner opposite-corner)
  (cons corner opposite-corner))

(define (corner r) (car r))
(define (opposite-corner r) (cdr r))

; width, heightの求め方は異なる
(define (width r)
  (abs (- (x-point (corner r)) (x-point (opposite-corner r)))))

(define (height r)
  (abs (- (y-point (corner r)) (y-point (opposite-corner r)))))

(define a
  (make-point 0 0))

(define b
  (make-point 4 2))

(define r
  (make-rectangle a b))

; 長さを直接指定する場合
(define (make-rectangle width height)
  (cons width height))

(define (width r) (car r))
(define (height r) (cdr r))

(define r
  (make-rectangle 3 4))

(perimeter r)
(area r)

