#lang sicp
; 1.23
; 本節の初めのsmallest-divisorは多くの不要な計算をする: 数が2で割り切れるか調べた後は, より大きい偶数で割り切れるか調べるのは無意味である.
; test-divisorが使う値は, 2, 3, 4, 5, 6, ...ではなく, 2, 3, 5, 7, 9, ...であるべきだ.
; この変更を実装するため, 入力が2なら3 を返し, そうでなければ入力に2足したものを返す手続きnextを定義せよ.
; smallest-divisorを(+ test-divisor 1)ではなく, (next test-divisor)を使うように修正せよ.
; このように修正した smallest-divisorにしたtimed-prime-testで, 問題1.22で見つけた12 個の素数をテストせよ.
; この修正はテストのステップを半分にしたので, 二倍速く走ることが期待される.
; この期待は確められるか.
; そうでなければ, 得られた二つのアルゴリズムの速度の比はどうであったか.
; それが2と違う事情を説明せよ.

; 1.21まで

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))


(define (square x)
  (* x x))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

; 今回作ったnext
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

; 1.22の処理
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (display n)
  ; (display " *** ")
  (display " ")
  (display elapsed-time)
  (newline))

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

(search-for-primes 10000)
(search-for-primes-2 10000)

; ---
; start
; 10007 3
; 10009 3
; 10037 3
; "end"
; start
; 9973 3
; 9967 3
; 9949 3
; "end"
; ---

(search-for-primes 10000)


