#lang sicp

; a. sumと(問題1.31の)productは, 一般的なアキュムレーションの関数:
; (accumulate combiner null-value term a next b)
; を使い, 項の集りを組み合せるaccumulateという更に一般的なものの特殊な場合であることを示せ.
; accumulateは引数としてsumや productと同様, 項と範囲指定と, 先行する項のアキュムレーションと現在の項をどう組み合せるかを指定する(二引数の)combiner手続き, 項がなくなった時に使う値を指定するnull-valueをとる.
; accumulateを書き, sumやproductがaccumulateの単なる呼出しで定義出来ることを示せ.

(define (accumulate combiner null-value term a next b)
  (define (s term a)
    (if (> a b)
        null-value
        (combiner (term a)
                  (s term (next a)))))
  (s term a))

; sumの動作テスト

(define (inc n) (+ n 1))
(define (cube x) (* x x x))

(define (sum-cubes a b)
  (accumulate + 0 cube a inc b))

(sum-cubes 0 10)
; 3025
; 良さそう

; productの動作テスト

(define (none x)
  x)

(define (factorial n)
  (accumulate * 1 none 1 inc n))

(factorial 6)
; 720
; 良さそう

; b. 上のaccumulateが再帰的プロセスを生成するなら, 反復的プロセスを生成するものを書け. 反復的プロセスを生成するなら, 再帰的プロセスを生成するものを書け.
(define (accumulate-2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


; sumの動作テスト

(define (sum-cubes-2 a b)
  (accumulate-2 + 0 cube a inc b))

(sum-cubes-2 0 10)
; 3025
; 良さそう

; productの動作テスト
(define (factorial-2 n)
  (accumulate-2 * 1 none 1 inc n))

(factorial-2 6)
; 720
; 良さそう
