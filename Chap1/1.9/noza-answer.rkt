#lang racket

(define (inc a)
  (+ a 1))

(define (dec a)
  (- a 1))

(define (add-1 a b)
  (if (= a 0)
    b
    (inc (add-1 (dec a) b))))

(define (add-2 a b)
  (if (= a 0)
    b
    (add-2 (dec a) (inc b))))

(add-1 4 5)
; (if (= 4 0)
;   5
;   (inc (add-1 (dec 4) 5))) 
; (inc (add-1 (dec 4) 5)) 以下 add-1 の取り出しと if の評価は省略
; (inc (add-1 (- 4 1) 5))
; (inc (add-1 3 5))
; (inc (inc (add-1 2 5)
; (inc (inc (inc (add-1 1 5)))
; (inc (inc (inc (inc (add-1 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

; 置き換えモデルの膨張と演算が実際に起こるときに収縮しているため再帰的である

(add-2 4 5)
; (if (= 4 0)
;   5
;   (add-2 (dec 4) (inc 5))))
; (add-2 (dec 4) (inc 5))
; (add-2 (- 4 ) (+ 5 1))
; (add-2 3 6) ; 以下 add-2 の取り出しと if の評価は省略
; (add-2 2 7)
; (add-2 1 8)
; (add-2 0 9)
; 9

; 状態変数a,bを保持しておけば計算できるため反復的プロセスである
