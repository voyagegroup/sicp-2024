#lang racket

; 問題分のコピー
(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
      (remainder (* (expmod base (/ exp 2) m)
                 (expmod base (/ exp 2) m))
                 m))
    (else
      (remainder (* base (expmod base (- exp 1) m)) m))))

; (remainder (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m)
; の部分が計算量を増加させている
;
; (/ exp 2)でexpが1/2ずつ減るためθ(log n)になっているが、expmodが2回呼ばれているためexpmodの手続きが2倍になっている
; そのため、1/2 * 2 * n 回だけ手続きが行われるため、θ(n)の計算量となってしまっている
