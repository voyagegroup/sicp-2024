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


; ---- 2.3 ここから
; 長方形の構成子と選択子
(define (make-rectangle width-segment height-segment)
  (cons width-segment height-segment))

(define (width-rectangle rectangle)
  (car rectangle))

(define (height-rectangle rectangle)
  (cdr rectangle))

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

; 確認
(define width-start-point (make-point 0 0))
(define width-end-point (make-point 5 0))
(define width-segment (make-segment width-start-point width-end-point))

(define height-start-point (make-point 0 0))
(define height-end-point (make-point 0 3))
(define height-segment (make-segment height-start-point height-end-point))

(define my-rectangle (make-rectangle width-segment height-segment))

(perimeter my-rectangle)
; -> 16
(area my-rectangle)
; -> 15















