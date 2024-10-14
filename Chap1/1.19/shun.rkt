;Fibonacci数を対数的ステップ数で計算するうまいアルゴリズムがある. 1.2.2 節のfib-iterプロセスで使った状態変数aとbの変換: a ← a + bとb ← aに注意しよう. この変換をTと呼ぶ. 1と0から始め, Tを繰り返してn回作用させると, Fib(n + 1)とFib(n)の対が出来る. いいかえれば, Fibonacci数は対(1, 0)にTn, つまり変換Tのn乗を作用させると得られる. さて, Tpqは対(a, b)をa ← bq + aq + apとb ← bp + aqに従って変換するものとし, Tを変換族Tpqのp = 0とq = 1の特例としよう. 変換Tpqを二回使うとその効果は同じ形式の変換Tp'q'を一回使ったのと同じになることを示し, p'とq'をp, qを使って表せ. これで変換を二乗する方法が得られ, fast-exptのように逐次平方を使い, Tnを計算することが出来る. これらをすべてまとめ, 対数的ステップ数の以下の手続きを完成せよ.

#lang racket

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
        (fib-iter a
                  b
                  (+ (* p p) (* q q))      ; p'を計算
                  (+ (* 2 (* p q)) (* q q))      ; q'を計算
                  (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 6)

;変換Tpqを１回適用する。

; a' = bq + aq + ap
; b' = bp + aq

;変換Tpqを２回適用する。

; a'' = b'q + a'q + a'p
; b'' = b'p + a'q

;変換Tpqの結果を代入し、a''とb''をaとbとpとqで表し、整理する。

; a'' = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
; b'' = (bp + aq)p + (bq + aq + ap)q

; a'' = bpq + aqq + bq + aqq + aqq + apq + bq + aqq + apq + apq
; b'' = bpp + aqq + bq + aqq + apq

; a'' = a(qq + qq + qq + pq + qq + pq + pq) + b(pq + qq + pq)
; b'' = a(qq + qq + qq + pq + qq + pq) + b(pp + qq)

; a'' = a(pp + 2pq + 2qq) + b(2pq + qq)
; b'' = a(2pq + qq) + b(pp + qq)

; Tpqを２回適用すると、Tp'q'を１回適用したのと同じになるようなp'とq'を求める。

; p' = pp + 2pq + 2qq
; q' = 2pq + qq
