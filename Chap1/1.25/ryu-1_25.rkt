#lang sicp

; Alyssa P. Hackerはexpmodを書くのに多くの余分なことをやったと不満で, 結局べき乗の計算法は知っているから
; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))
; と単純に書ける筈だといった. これは正しいか. これも高速素数テストと同じに使えるか, 説明せよ.

(define (square x) (* x x))

; もともとのexpmod
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; memo
; baseはランダムな値。expとmの初期値は同じ値

; ----------- もともとのexpmodで (expmod 3 5 5) の手続きを追ってみる ---------------
(expmod 3 5 5)
; (= 5 0) -> #f
; (even? 5) -> #f
(remainder (* 3 (expmod 3 (- 5 1) 5)) 5)
(remainder (* 3 (expmod 3 4 5)) 5)
; (expmod 3 4 5)
; (= 4 0) -> #f
; (even? 4) -> #t
; (expmod 3 4 5) -> (remainder (square (expmod 3 (/ 4 2) 5)) 5)
(remainder (* 3 (remainder (square (expmod 3 2 5)) 5)) 5)
; (expmod 3 2 5)
; (= 2 0) -> #f
; (even? 2) -> #t
; (expmod 3 2 5) -> (remainder (square (expmod 3 (/ 2 2) 5)) 5)
(remainder (* 3 (remainder (square (remainder (square (expmod 3 1 5)) 5)) 5)) 5)
; (expmod 3 1 5)
; (= 1 0) -> #f
; (even? 1) -> #f
; (expmod 3 1 5) -> (remainder (* 3 (expmod 3 (- 1 1) 5)) 5)
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 (expmod 3 0 5)) 5)) 5)) 5)) 5)
; (expmod 3 0 5)
; (= 0 0) -> #t
; (expmod 3 0 5) -> 1
(remainder (* 3 (remainder (square (remainder (square (remainder (* 3 1) 5)) 5)) 5)) 5)
(remainder (* 3 (remainder (square (remainder (square (remainder 3 5)) 5)) 5)) 5)
(remainder (* 3 (remainder (square (remainder (square 3) 5)) 5)) 5)
(remainder (* 3 (remainder (square (remainder 9 5)) 5)) 5)
(remainder (* 3 (remainder (square 4) 5)) 5)
(remainder (* 3 (remainder 16 5)) 5)
(remainder (* 3 1) 5)
(remainder 3 5)
3




; --- 1.2.4 からもってきた逐次平方

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; 今回の問ででてきた手続き
(define (expmod-v2 base exp m)
  (remainder (fast-expt base exp) m))


; ----------- 今回のexpmod-v2で (expmod 3 5 5) の手続きを追ってみる ---------------
(expmod-v2 3 5 5)
(remainder (fast-expt 3 5) 5)
; (faxt-exp 3 5)
; (= 5 0) -> #f
; (even? 5) -> #f
; (faxt-exp 3 5) -> (* 3 (fast-expt 3 (- 5 1)))
(remainder  (* 3 (fast-expt 3 4)) 5)
; (fast-exp 3 4)
; (= 4 0) -> #f
; (even? 4) -> #t
; (fast-exp 3 4) -> (square (fast-expt 3 (/ 4 2)))
(remainder  (* 3 (square (fast-expt 3 2))) 5)
; (fast-expt 3 2)
; (= 2 0) -> #f
; (even? 2) -> #t
; (fast-expt 3 2) -> (square (fast-expt 3 (/ 2 2)))
(remainder  (* 3 (square (square (fast-expt 3 1)))) 5)
; (fast-expt 3 1)
; (= 1 0) -> #f
; (even? 1) -> #f
; (fast-expt 3 1) -> (* 3 (fast-expt 3 (- 1 1)))
(remainder  (* 3 (square (square (* 3 (fast-expt 3 0))))) 5)
; (fast-expt 3 0)
; (= 0 0) -> #t
; (fast-expt 3 0) -> 1
(remainder  (* 3 (square (square (* 3 1)))) 5)
(remainder  (* 3 (square (square 3))) 5)
(remainder  (* 3 (square 9)) 5)
(remainder  (* 3 81) 5)
(remainder  243 5)
3

; --- 回答 ---
; 正しいかの問は、理論上は、正しい値がちゃんと出力されるはずである。
; これも高速素数テストと同じに使えるかの問は、使えないと考える。
; もともとの方は、随時remainderを行う。
; v2の方は、最終的なb^nのあたいにたいして1回remainderを行う。
; その結果、b^nが大きい値となる場合、オーバーフローの可能性がある。
; 加えて、オーバーフローしなかったとしても計算時間に差がでてしまう。
; 実際、以下の結果は、もともとの方は一瞬だが、v2は時間がかかってしまう。

(expmod 7 10000000 10000000)
(expmod-v2 7 10000000 10000000)


