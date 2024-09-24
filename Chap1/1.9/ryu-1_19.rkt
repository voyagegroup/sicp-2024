#lang sicp
; 1.9
; 1つ目
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
; inc -> 引数を1増やす
; dec -> 引数を1減らす

(+ 4 6)
(if (= 4 0)
    0
    (inc (+ dec 4) 0))
; if文を評価
(= 4 0)
; -> #f
(inc (+ (dec 4) 6))
(inc (+ 3 6))
(inc
 (if (= 3 0)
     0
     (inc (+ (dec 3) 6))))
; if文を評価
(= 3 0)
; -> #f
(inc (+ (dec 3) 6))
(inc (+ 2 6))
; -> (inc (inc (+ 2 6)))
(inc
 (inc
  (if (= 2 0)
      6
      (inc (+ (dec 2) 6)))))
(= 2 0)
; -> #f
(inc (+ (dec 2) 6))
(inc (+ 1 6))
; -> (inc (inc (inc (+ 1 6)))
(inc
 (inc
  (inc
   (if (= 1 0)
       6
       (inc (+ (dec 1) 6))))))
(= 1 0)
; -> #f
(inc (+ (dec 1) 6))
(inc (+ 0 6))
; -> (inc (inc (inc (inc (+ 0 6)))))
(inc
 (inc
  (inc
   (inc
    (if (= 0 0)
        6
        (inc (+ dec 0) 6))))))
(= 0 0)
; -> #t
6
(inc (inc (inc (inc 6))))
(inc (inc (inc 7)))
(inc (inc 8))
(inc 9)
10
; 再帰的

; 2つ目
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 6)
; if文を評価
(= 4 0)
; -> #f
(+ (dec 4) (inc 6))
(+ 3 7)
(if (= 3 0)
    7
    (+ (dec 3) (inc 7)))
; if文を評価
(= 3 0)
; -> #f
(+ (dec 3) (inc 7))
(+ 2 8)
(if (= 2 0)
    8
    (+ (dec 2) (inc 8)))
; if文を評価
(= 2 0)
; -> #f
(+ (dec 2) (inc 8))
(+ 1 9)
(if (= 1 0)
    9
    (+ (dec 1) (inc 9)))
; if文を評価
(= 1 0)
; -> #f
(+ (dec 1) (inc 9))
(+ 0 10)
(if (= 0 0)
    10
    (+ (dec 0) (inc 10)))
; if文を評価
(= 0 0)
10
; 反復的
