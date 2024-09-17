;

#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; 作用的順序
; (test 0 (p))
; 部分式の (p) を評価する
; (define (p) (p)) なので、 (p) となる
; (p) を評価する
; 以下ループ

; 正規順序
; (test 0 (p))
; (if (= 0 0) 0 (p)) testを置き換える
; (if #t 0 (p)) ifを評価する
; 0

