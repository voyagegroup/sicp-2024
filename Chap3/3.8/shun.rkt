#lang sicp

(define f
  (let ((ini 1))
    (lambda (n)
      (begin (set! ini (* n ini)) ini))))

(+ (f 0) (f 1))

; 左から右に評価する時
; (f 0)
; ((let ((ini 1)) (lambda (n) (begin (set! ini (* n ini)) ini))) 0)
; ((lambda (n) (begin (set! ini (* n ini)) ini)) 0)
; (begin (set! ini (* 0 ini)) ini)
; (begin (set! ini (* 0 1)) ini)
; (begin (set! ini 0) ini)
; (begin ini) // iniには0がsetされている
; (begin 0)
; (f 0) = 0
; (+ 0 (f 1)) // 元の式に代入
; (f 1)
; ((let ((ini 0)) (lambda (n) (begin (set! ini (* n ini)) ini))) 1)
; ((lambda (n) (begin (set! ini (* n ini)) ini)) 1)
; (begin (set! ini (* 1 ini)) ini)
; (begin (set! ini (* 1 0)) ini)
; (begin (set! ini 0) ini)
; (begin ini) // iniには0がsetされている
; (begin 0)
; (f 1) = 0
; (+ 0 0)
; 0


; 右から左に評価する時
; (f 1)
; ((let ((ini 1)) (lambda (n) (begin (set! ini (* n ini)) ini))) 1)
; ((lambda (n) (begin (set! ini (* n ini)) ini)) 1)
; (begin (set! ini (* 1 ini)) ini)
; (begin (set! ini (* 1 1)) ini)
; (begin (set! ini 1) ini)
; (begin ini) // iniには1がsetされている
; (begin 1)
; (f 1) = 1
; (+ (f 0) 1)
; (f 0)
; ((let ((ini 1)) (lambda (n) (begin (set! ini (* n ini)) ini))) 0)
; ((lambda (n) (begin (set! ini (* n ini)) ini)) 0)
; (begin (set! ini (* 0 ini)) ini)
; (begin (set! ini (* 0 1)) ini)
; (begin (set! ini 0) ini)
; (begin ini) // iniには0がsetされている
; (begin 0)
; (f 0) = 0
; (+ 0 1)
; 1