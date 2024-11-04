#lang sicp
; 脚注47のCarmichael数が本当にFermatテストをだますことを示せ. それには整数nをとり, a < nなるすべてのaで, anがnを法としてaの合同になるかどうか見る手続きを書き, Carmichael数でその手続きを使ってみる.

; 注釈47: Fermatテストをだます数は, Carmichael 数(Carmichael numbers)といい, 非常に稀だということ以外はよくわからない.
; 100,000,000以下のCarmichael数は255個あり, 小さい方のいくつかは561, 1105, 1729, 2465, 2821および6601である.
; ランダムに選んだ非常に大きい数の素数性のテストで, Fermatテストをだます数に遭遇する確率は, 宇宙線が計算機の「正しい」アルゴリズムに誤りを生じさせる確率より小さい.
; アルゴリズムが前の理由から不適切であり, 後の理由からとしないのは, 数学と工学の違いを見せる.

(define (square x) (* x x))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 561 100)
; -> #t
(fast-prime? 1105 100000)
; -> #t


; ------ 全部の数値においてチェックするやつ --------
(define (fast-prime-check-all n)
  (fast-prime-all? n (- n 1)))

(define (fast-prime-all? n times)

  (cond ((= times 0) true)
        ((fermat-test-n n times) (fast-prime-all? n (- times 1)))
        (else false)))

(define (fermat-test-n n a)
  (= (expmod a n n) a))

(fast-prime-check-all 3)
; -> #t
(fast-prime-check-all 199)
; -> #t
(fast-prime-check-all 1999)
; -> #t
(fast-prime-check-all 19999)
; -> #f
; 手続きの挙動はただしそう

; Carmichael数で確認してみる
(fast-prime-check-all 561)
; -> #t
(fast-prime-check-all 1105)
; -> #t
(fast-prime-check-all 1729)
; -> #t
(fast-prime-check-all 2465)
; -> #t
(fast-prime-check-all 2821)
; -> #t
(fast-prime-check-all 6601)
; -> #t

