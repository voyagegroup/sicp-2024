#lang sicp

; --- fixed-point

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

(fixed-point cos 1.0)
; -> 0.7390822985224023

; --- 平均緩和法
(define (sqrt-old x)
  (fixed-point (lambda (y) (/ x y))
               1.0))
; (sqrt-old 2)
; -> 無限ループ

(define (average a b) (/ (+ a b) 2.0))
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 2)
; -> 1.4142135623746899


; --- average-damp
; average-dampは引数として手続きfをとり, その値として(lambdaで作り出された)手続き, つまりある数xに作用させるとx と(f x)の平均を返す手続き, を返す手続きである.
(define (square x) (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)
; -> 55.0


; --- repeated

(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f x)
  (if (= x 1)
      f
      (compose f (repeated f (- x 1)))))

((repeated square 2) 5)
; -> 625


; --- 本題
; まず3乗根を目指してみるか
; (cube-root 27) -> 3 になるような手続きを目指す
; y |-> x/y^n-1 で、cubeなので、x/y^2
; cubeなので、複数回平均緩和をつかわなくてもとけるはず
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(cube-root 27)
; -> 2.9999972321057697
; よさそう

; つぎは4乗根をめざしてみる
; (fourth-root 81) -> 3
(define (cube x) (* x x x))
; (define (fourth-root x)
;   (fixed-point
;    (average-damp (lambda (y) (/ x (cube y))))
;    1.0))
; (fourth-root 81)
; 終わらない

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y)))) 1.0))

(fourth-root 81)
; -> 3.000000000000033
; よさそう


; Q1. y  x/yn-1の平均緩和を繰り返し使った不動点探索として n乗根を計算するのに, 何回の平均緩和が必要か実験してみよ.

; b^n をらくにしたいので引っ張ってきた
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(fast-expt 2 3)
; -> 2^3 = 8

; (fifth-root 243) -> 5
(define (fifth-root-2 x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (fast-expt y 4)))) 1.0))
(fifth-root-2 243)
; -> 3.0000008877496294

(define (fifth-root-3 x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (fast-expt y 4)))) 1.0))
(fifth-root-3 243)
; -> 3.000003432225565

(define (fifth-root-4 x)
  (fixed-point ((repeated average-damp 4) (lambda (y) (/ x (fast-expt y 4)))) 1.0))
(fifth-root-4 243)
; -> 3.000017342035047

(define (fifth-root-5 x)
  (fixed-point ((repeated average-damp 5) (lambda (y) (/ x (fast-expt y 4)))) 1.0))
(fifth-root-5 243)
; -> 3.0000526794931255

  
; なんか誤差の範囲な気もするけど、どんどん遠ざかってない？
; 2回で十分ってことなのかな？
; 100乗根でためしてみるか。

; (handredth-root 10000) は、 約 1.096478196143185 になるらしい

(define (hundredth-root-2 x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (fast-expt y 99)))) 1.0))
(hundredth-root-2 10000)
; -> 1.0964775679357073
; 大体あっている

(define (hundredth-root-100 x)
  (fixed-point ((repeated average-damp 100) (lambda (y) (/ x (fast-expt y 99)))) 1.0))
(hundredth-root-100 10000)
; -> 1.0
; おや?1.0になってしまった。多すぎてもだめなのかな。

(define (hundredth-root-50 x)
  (fixed-point ((repeated average-damp 50) (lambda (y) (/ x (fast-expt y 99)))) 1.0))
(hundredth-root-50 10000)
; -> 1.000000000008881
; 50にしてみたけど、やっぱり遠ざかってるな。

; 2回で十分なのは、すべてのn乗根においてなのかな？
; 一旦、2回で十分と過程して、進めてみるか。

; (define (n-root n x)) みたいになるイメージ
; (n-root 4 81) -> 3が出力される

(define (n-root n x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0))
(n-root 4 81)
; -> 3.000000000000033
(n-root 2 4)
; -> 1.999993326908296











