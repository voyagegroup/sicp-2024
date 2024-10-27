#lang sicp
; 1.22
; 殆んどのLispの実装には基本手続きruntimeがあり, システムが走った時間を(例えばマイクロ秒で)示す整数を返す.
; 次のtimed-prime-test手続きは整数nで呼び出されると, nを印字し, nが素数かどうかを調べ, nが素数なら手続きは三つの星印とテストに要した時間を印字する.

; 1.21までの手続き

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))


(define (square x)
  (* x x))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
  (= (remainder b a) 0))

; 1.22ででてきた手続き

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display " ")
  (display elapsed-time)
  (newline))

;(timed-prime-test 19999999)
; -> 19999999 *** 152
;(timed-prime-test 19999)
; -> 19999

; この手続きを使い, 指定範囲の連続する奇整数について素数性を調べる手続きsearch-for-primesを書け.
; その手続きで, それぞれ1,000, 10,000, 100,000, 1,000,000より大きい, 小さい方から三つの素数を見つけよ.
; 素数をテストする時間に注意せよ. テストのアルゴリズムはΘ()の増加の程度だから, 10,000前後の素数のテストは1,000前後の素数のテストの倍かかると考えよ.
; 時間のデータはこれを支持するか. 100,000, 1,000,000のデータは予想のとおりだろうか.
; 結果はプログラムが計算に必要なステップ数に比例した時間で走るという考えに合っているか.



;(define (start-prime-test-2 n start-time count)
  ;(if (prime? n)
      ;(report-prime (- (runtime) start-time)) (+ count 1)))


; 大きいの3つを探す処理
(define (start-prime-test-2 display n count)
  (cond ((> count 3) "end")
        ((prime? n) (start-prime-test-2 (timed-prime-test n) (+ n 1) (+ count 1)))
        (else (start-prime-test-2 (timed-prime-test n) (+ n 1) count))))

(define (search-for-primes n)
  (start-prime-test-2 (display "start\n") n 1))


(define (start-prime-test-3 display n count)
  (cond ((> count 3) "end")
        ((prime? n) (start-prime-test-3 (timed-prime-test n) (- n 1) (+ count 1)))
        (else (start-prime-test-3 (timed-prime-test n) (- n 1) count))))

(define (search-for-primes-2 n)
  (start-prime-test-3 (display "start\n") n 1))

(search-for-primes 100000000000)
(search-for-primes-2 100000000000)











        