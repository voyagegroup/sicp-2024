#lang sicp
; Louis Reasonerは問題1.24がなかなかうまくいかなかった. 彼の fast-prime?はprime?よりずっと遅いようであった. Louisは友人のEva Lu Atorに助けを求めた. Louisのプログラムを見ていると, squareを呼び出す代りに乗算を陽に使うように変っていた.
; 「違いが分らん」とLouis, 「分る」とEvaがいった. 「手続きをこのように書き替えたので, Θ(log n)のプロセスをΘ(n)のプロセスにしてしまった.」 説明せよ.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))

                    m))))

; 手続きを追ってみる
(expmod 3 5 5)
; (= 5 0) -> #f
; (even? 5) -> #f
; (expmod 3 5 5) -> (remainder (* 3 (expmod 3 (- 5 1) 5)) 5)
(remainder (* 3 (expmod 3 4 5)) 5)
; (expmod 3 4 5)
; (= 4 0) -> #f
; (even? 4) -> #t
; (expmod 3 4 5) -> (remainder (* (expmod 3 (/ 4 2) 5) (expmod 3 (/ 4 2) 5)) 5)
(remainder (* 3 (remainder (* (expmod 3 2 5) (expmod 3 2 5)) 5)) 5)

; もともとの手続きと比較してみる
; 今回: (remainder (* 3 (remainder (* (expmod 3 2 5) (expmod 3 2 5)) 5)) 5)
; もともと: (remainder (* 3 (remainder (square (expmod 3 2 5)) 5)) 5)
; (expmod 3 2 5) を2回計算することになっている
; expmodは自身を2回呼ぶことになるので、n^2だけ倍々に増加していくことになる
; 結果、θ(log_2 n^2) = θ(n) となる



