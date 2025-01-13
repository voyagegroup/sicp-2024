#lang sicp

(define zero (lambda (f) (lambda (x) x)))
; -> xをそのままかえしているだけ

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (x))))
; -> (f) が1回実行されるようになった

; --- oneを定義 ---
(define one (lambda (f) (lambda (x) (f x))))


; --- (add-1 one) の手続きを追ってみる ---
(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (f x))))

; --- twoを定義 ---
(define two (lambda (f) (lambda (x) (f (f x)))))

; --- 動作を確認するために、2倍にする手続きを定義 ---
(define two-times (lambda (n) (* n 2)))

; --- one, two, add-1 の動作を確認してみる
(two-times 3)
; -> 6
((one two-times) 3)
; -> 6
((two two-times) 3)
; -> 12
(((add-1 one) two-times) 3)
; -> 12
(((add-1 zero) two-times) 3)
; -> 6

; --- + を定義 ---
(define (+ m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(+ one one)
; -> (lambda (f) (lambda (x) ((one f) ((one f) x))))

(((+ one zero) two-times) 3)
; -> 6
(((+ one one) two-times) 3)
; -> 12
(((+ one two) two-times) 3)
; -> 24
(((+ two one) two-times) 3)
; -> 24
(((+ two two) two-times) 3)
; -> 48

; --- 置き換えモデルで手続きを追ってみる ---
(((+ one one) two-times) 3)

; (+ one one)
; -> (lambda (f) (lambda (x) ((one f) ((one f) x))))
(((lambda (f) (lambda (x) ((one f) ((one f) x)))) two-times) 3)

; (f) = two-times
((lambda (x) ((one two-times) ((one two-times) x))) 3)
; (x) = 3
((one two-times) ((one two-times) 3))

; (one two-times)
; -> (lambda (x) (two-times x))
((lambda (x) (two-times x)) ((lambda (x) (two-times x)) 3))

; ((lambda (x) (two-times x)) 3)
; -> 6
((lambda (x) (two-times x)) 6)

(two-times 6)
; -> 12




