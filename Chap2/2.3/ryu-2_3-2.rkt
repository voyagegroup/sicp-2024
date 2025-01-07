#lang sicp

; ---- 2.2 で作ったやつ
; 線分の表現を定義する構成子
(define (make-segment start end)
  (cons start end))

; 選択子
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; 点は一対の数: x座標とy座標で表現する構成子
(define (make-point x y) (cons x y))

; 選択子
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; 2_3.rktからコピペ

; 長方形の周囲の長さを計算
(define (perimeter rectangle)
  (* 2 (+ (segment-length (width-rectangle rectangle))
          (segment-length (height-rectangle rectangle)))))

; 長方形の面積を計算
(define (area rectangle)
  (* (segment-length (width-rectangle rectangle))
     (segment-length (height-rectangle rectangle))))

; 線分の長さを計算
; √(x2 - x1)^2 + (y2 - y1)^2)
(define (square x) (* x x))
(define (segment-length segment)
  (let ((x1 (x-point (start-segment segment)))
        (y1 (y-point (start-segment segment)))
        (x2 (x-point (end-segment segment)))
        (y2 (y-point (end-segment segment))))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

; ---- 実装2
;左下と右上の点で表現
(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))

(define (width-rectangle rectangle)
  (make-segment
   (make-point (x-point (car rectangle)) (y-point (car rectangle)))
   (make-point (x-point (cdr rectangle)) (y-point (car rectangle)))))

(define (height-rectangle rectangle)
  (make-segment
   (make-point (x-point (car rectangle)) (y-point (car rectangle)))
   (make-point (x-point (car rectangle)) (y-point (cdr rectangle)))))

; --- 確認
(define bottom-left (make-point 0 0))
(define top-right (make-point 5 3))
(define my-rectangle (make-rectangle bottom-left top-right))

(perimeter my-rectangle)
; -> 16
(area my-rectangle)
; -> 15
