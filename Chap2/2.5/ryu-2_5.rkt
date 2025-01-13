#lang sicp

; aとbの対を積2^a*3^bである整数で表現するなら, 非負の整数の対は数と算術演算だけを使って表現出来ることを示せ.
; これに対応する手続き cons, carおよびcdrの定義は何か.

; e.g. (a, b) = (3, 4)の場合を考えてみる
; 2^3 * 3^4 = 8 * 81 = 648
; (a, b) を求める
; 648を2と3でそれぞれ素因数分解をしていく
; 648を2で素因数分解
; 2 * 2 * 2 * 81
; 648 を3で素因数分解
; 3 * 3 * 3 * 3 * 8
; -> a = 3, b = 4


(define (cons a b)
  (* (expt 2 a) (expt 3 b)))
(cons 3 4)
; -> 648

(define (car z)
  (define (dispatch z count) ; %2!=0 になるまで/2をしながらcountをする
    (if (= (remainder z 2) 0)
        (dispatch (/ z 2) (+ count 1))
        count))
  (dispatch z 0))

(define (cdr z)
  (define (dispatch z count)
    (if (= (remainder z 3) 0)
        (dispatch (/ z 3) (+ count 1))
        count))
  (dispatch z 0))

(car (cons 3 4))
; -> 3
(cdr (cons 3 4))
; -> 4

(car (cons 10 12))
; -> 10
(cdr (cons 10 12))
; -> 12
