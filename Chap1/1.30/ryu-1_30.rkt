#lang sicp
; 上のsumの手続きは線形再帰を生成する. 総和が反復的に実行出来るように手続きを書き直せる. 次の定義の欠けているところを補ってこれを示せ:
; 線形再帰のsum
; (define (sum term a next b)
;   (if (> a b)
;       0
;       (+ (term a)
;          (sum term (next a) next b))))

; 反復的なsum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


; 動作検証

(define (cube x) (* x x x))

(define (inc n) (+ n 1))


(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)
(sum cube 1 inc 10)
; (iter 1 0)
; -> (iter (next 1) (+ (cube 1) 0))
; (iter 2 1)
; -> (iter (next 2) (+ (cube 2) 1))
; (iter 3 9)
; -> (iter (next 3) (+ (cube 3) 9))
; ....
3025

; 反復的になっている
