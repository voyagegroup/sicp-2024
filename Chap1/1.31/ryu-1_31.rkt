#lang sicp
; a. sum手続きは高階手続きとして書ける, 同様な数多い抽象の最も単純なものに過ぎない.51 与えられた範囲の点での関数値の積を返すproductという似た手続きを書け. productを使って, factorialを定義せよ.
; また式
; // 画像になっていてとれない
; によってπの近似値を計算するのにproductを使え

; memo
; 高階手続き → 手続きを扱う手続き
; factorial → 階乗

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))


; factorialの実装

(define (inc n) (+ n 1))

(define (none x)
  x)

(define (factorial n)
  (product none 1 inc n))

(factorial 3)
; 6
(factorial 6)
; 720

; πの近似値

(define (term-pi n)
  (if (even? n)
      (/ (+ n 2.0) (+ n 1.0))
      (/ (+ n 1.0) (+ n 2.0))))

(* (product term-pi 1 inc 10000) 4)
; 3.1417497057380084


; b. 上のproductが再帰的プロセスを生成するなら, 反復的プロセスを生成するものを書け. 反復的プロセスを生成するなら, 再帰的プロセスを生成するものを書け.

; 反復的プロセスを書いたので、再帰的プロセスを書く


(define (product-2 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-2 term (next a) next b))))

(define (factorial-2 n)
  (product-2 none 1 inc n))

(factorial-2 6)
; 720

(* (product-2 term-pi 1 inc 10000) 4)
; 3.1417497057379635
