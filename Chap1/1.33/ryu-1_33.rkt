#lang sicp

; 組み合せる項に フィルタ(filter)の考えを入れることで, accumulate(問題1.32)の更に一般的なものが得られる.
; つまり範囲から得られて, 指定した条件を満した項だけを組み合せる.
; 出来上ったfiltered-accumulate抽象は, accumulate と同じ引数の他, フィルタを指定する一引数の述語をとる. filtered-accumulateを手続きとして書け.
; filtered-accumulateを使い, 次をどう表現するかを示せ.



; いつもの

(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (none x) x)

; filterを追加したaccumulate

(define (accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) (combiner null-value result)))))
  (iter a null-value))

; 偶数を足す
(accumulate + 0 even? none 0 inc 10)
; -> 30
(+ 0 2 4 6 8 10)
; -> 30


; a. 区間a, bの素数の二乗の和(prime?述語は持っていると仮定する.)
; (prime?)
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))

; 素数を足す
(accumulate + 0 prime? none 0 inc 10)
; -> 18
(+ 0 1 2 3 5 7)
; -> 18
; 動作良さそう

(accumulate + 0 prime? square 1 inc 10)
; -> 88
(+ (square 1) (square 2) (square 3) (square 5) (square 7))
; -> 88

; b. nと互いに素で, nより小さい正の整数(つまりi < nでGCD(i, n)=1なる全整数i)の積


(define (b n)
  (define (b-filter? i)
    (= (gcd n i) 1))
  (accumulate * 1 b-filter? none 1 inc n))
  
(b 10)
; -> 189
(* 1 3 7 9)
; -> 189



