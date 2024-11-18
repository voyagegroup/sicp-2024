#lang sicp

; h = (b - a) / n
; y_k = f(a+kh)
; [a, b] 区間
; k = aからbまでn等分されたインデックス
; S = y_0 + 4(y_1) + 2(y_2) + 4(y_3) + ... + yn
; h/3 * S

(define (cube x) (* x x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (h a b n)
  (/ (- b a) n))

(define (s k f a h n)
  (cond 
    ((= k 0) 
     (+ (f a) (s (+ k 1) f a h n))) ; 初項
    ((= k n)
     (f (+ a (* k h)))) ; 最後の項
    ((even? k) 
     (+ (* 2.0 (f (+ a (* k h)))) (s (+ k 1) f a h n))) ; 偶数番目の項
    (else 
     (+ (* 4.0 (f (+ a (* k h)))) (s (+ k 1) f a h n))))) ; 奇数番目の項


(define (simpson f a b n)
  (* (/ (h a b n) 3.0) (s 0 f a (h a b n) n ))) ; sにh/3をかける


(simpson cube 0 1 100)
; 0.24999999999999992
(simpson cube 0 1 1000)
; 0.2500000000000002


; 結果を上のintegral手続きの結果と比較せよ.

; integralの結果
; (integral cube 0 1 0.01)
; -> 0.24998750000000042
; (integral cube 0 1 0.001)
; -> 0.249999875000001

; 本文より
; > 0と1の間のcubeの積分の正確な値は1/4である.
; simpsonの場合は、ほぼ0.25が出力された。

; memo
; sumを使って書き直さないと