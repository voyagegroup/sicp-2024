#lang racket

(define (cube x) (* x x x))

(define (p x)
  (display x)
  (newline)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 12.15)

; 0.049999999999999996
; 0.1495
; 0.4351345505
; 0.9758465331678772
; -0.7895631144708228
; -0.39980345741334

; pは6回作用させられた


; 置き換えモデル実行
; (sine a)
; (p (sine (/ a 3)))
; (p (p (sine (/ a 9.0))))
; (p (p (p (sine (/ a 27.0)))))

; p のステップ数は1。1回のステップで1/3になるので、ステップはΘ(log3 n)である。
; 状態として保持しておくものはないので、空間計算量はΘ(1)である。
