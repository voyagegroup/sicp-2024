#lang sicp

; 問題1.22のtimed-prime-test手続きをfast-prime?(Fermat法参照) を使うように変え, その問題で見つけた12個の素数をテストせよ.
; FermatテストはΘ(log n)の増加なので, 1,000,000近くの素数をテストする時間を1000近くの素数をテストする時間と比べ, どの位と予想するか.
; データはその予想を支持するか.
; 違いがあれば説明出来るか.


; --- FermatテストはΘ(log n)の増加なので, 1,000,000近くの素数をテストする時間を1000近くの素数をテストする時間と比べ, どの位と予想するか. ---
; log_10 1000 = 3
; log_10 1000000 = 6
; なので、1000:1000000=1:2 になると予想する

; --- fast-prime
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

; ---


(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display " ")
  (display elapsed-time)
  (newline))

(define (start-prime-test-2 display n count)
  (cond ((> count 3) "end")
        ((fast-prime? n 10) (start-prime-test-2 (timed-prime-test n) (+ n 1) (+ count 1)))
        (else (start-prime-test-2 (timed-prime-test n) (+ n 1) count))))

(define (search-for-primes n)
  (start-prime-test-2 (display "start\n") n 1))

; ---- 実行結果 ----

(search-for-primes 100)
; ----
; start
; 101 ***  6
; 103 ***  6
; 107 ***  6
; ---

(search-for-primes 1000)
; ---
; start
; 1009 ***  8
; 1013 ***  8
; 1019 ***  9
; "end"
; ---

(search-for-primes 1000000)
; ---
; start
; 1000003 ***  11
; 1000033 ***  11
; 1000037 ***  12
; "end"
; ---

(search-for-primes 10000000)
; ---
; start
; 10000019 ***  14
; 10000079 ***  14
; 10000103 ***  15
; "end"
; ---


(search-for-primes 100000000)
; ----
; start
; 100000007 ***  20
; 100000037 ***  21
; 100000039 ***  20
; "end"
; ---


(search-for-primes 1000000000)
; ---
; start
; 1000000007 ***  21
; 1000000009 ***  20
; 1000000021 ***  21
; "end"
; ---

; ---- 1000と1000000とを比較 ----
; 1000は約8
; 1000000は約15なので、概ね予想どおり、1:2になっていそう

