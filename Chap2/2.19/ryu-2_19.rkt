#lang sicp

; 1.2.2節からコピペ
(define (count-change amount)
  (cc-old amount 5))

(define (cc-old amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc-old amount
                     (- kinds-of-coins 1))
                 (cc-old (- amount
                        (first-denomination-old kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination-old kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; 問題文からコピペ
(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

; 解答ここから
(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))
(except-first-denomination us-coins)
; -> (15 10 5 1)

(define (first-denomination coin-values)
  (car coin-values))
(first-denomination us-coins)
; -> 50
  
(cc 100 us-coins) ; -> 292
; 手続きを追ってみる
(+ (cc 100 (except-first-denomination (list 50 25 10 5 1)))
   (cc ( - 100 (first-denomination (list 50 25 10 5 1))) (list 50 25 10 5 1)))
(+ (cc 100 (list 25 10 5 1))
   (cc ( - 100 50) (list 50 25 10 5 1)))
(+ (cc 100 (list 25 10 5 1))
   (cc 50 (list 50 25 10 5 1)))
; (cc 100 (list 25 10 5 1)) をみる
(+ (cc 100 (list 10 5 1))
   (cc (- 100 25) (list 25 10 5 1)))
; (cc 50 (list 50 25 10 5 1)) をみる
(+ (cc 50 (list 25 10 5 1))
   (cc (- 50 50) (list 50 25 10 5 1)))
; 2つを使う
(+ (+ (cc 100 (list 10 5 1)) ; memo 100を残りのコインで実行される
      (cc (- 100 25) (list 25 10 5 1)))
   (+ (cc 50 (list 25 10 5 1))
      (cc (- 50 50) (list 50 25 10 5 1))))
; ...

; リスト coin-valuesの順は, ccの答に影響があるか.
(define us-coins-revers (list 1 5 10 25 50))
(cc 100 us-coins-revers)
; -> 292

(define us-coins-random (list 10 1 50 5 25))
(cc 100 us-coins-random)
; -> 292

; 影響はない。
; なぜか
; ccの手続きは、listの数字の組み合わせを全部探索しているため、順番にかかわらず数えられる




