#lang racket

(define (make-point x y)
    (cons x y))

; 対角の頂点で表現

(define (make-rectangle p1 p2)
    (cons p1 p2))

(define (rectangle-p1 rectangle)
    (car rectangle))

(define (rectangle-p2 rectangle)
    (cdr rectangle))

(define (rectangle-perimeter rectangle)
    (let (
        (p1 (rectangle-p1 rectangle))
        (p2 (rectangle-p2 rectangle)))
        (* 2 (+ (abs (- (car p1) (car p2)))
            (abs (- (cdr p1) (cdr p2)))))))

(define (rectangle-area rectangle)
    (let (
        (p1 (rectangle-p1 rectangle))
        (p2 (rectangle-p2 rectangle)))
        (* (- (car p1) (car p2))
            (- (cdr p1) (cdr p2)))))

; 縦と横の長さで表現

(define (make-rectangle-2 yoko tate)
    (cons yoko tate))

(define (rectangle-yoko rectangle)
    (car rectangle))

(define (rectangle-tate rectangle)
    (cdr rectangle))

(define (rectangle-perimeter-2 rectangle)
    (let (
        (yoko (rectangle-yoko rectangle))
        (tate (rectangle-tate rectangle)))
        (* 2 (+ yoko tate))))

(define (rectangle-area-2 rectangle)
    (let (
        (yoko (rectangle-yoko rectangle))
        (tate (rectangle-tate rectangle)))
        (* yoko tate)))

; テスト
(rectangle-perimeter (make-rectangle (make-point 1 2) (make-point 4 4)))
(rectangle-area (make-rectangle (make-point 1 2) (make-point 4 4)))

(rectangle-perimeter-2 (make-rectangle-2 3 2))
(rectangle-area-2 (make-rectangle-2 3 2))

; 共通でやるためにはそれぞれの表現で縦と横を求めるような関数を作る、が定義の仕方がわからない・・・

; 周の長さは 2 * (縦 + 横)、面積は 縦 * 横 で求められるのでそれを引数にして、
; それぞれの表現で縦と横を求めるような関数を作るアプローチはあり
