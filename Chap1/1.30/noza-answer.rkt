#lang racket

; iterの定義
(define (it x) (+ x 1))

; cubeの定義
(define (cube x) (* x x x))

; sumの線形定義
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; sum の反復定義
(define (sum-ext term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum cube 0 it 100)
(sum-ext cube 0 it 100)

#| 反復になっているか確かめる |#
#| (sum-ext cube 0 iter 3) |#

#| (define (iter a result) |#
#|   (if (> a 3) |#
#|     result |#
#|     (iter (it a) (+ result (cube a))))) |#
#| (iter 0 0)) |#

#| (if (> 0 3) |#
#|   0 |#
#|   (iter (it 0) (+ 0 (cube a))))) |#

#| (iter (it 0) (+ 0 (cube a))))) |#

#| (iter 1 0) |#

#| (if (> 1 3) |#
#|   0 |#
#|   (iter (it 1) (+ 0 (cube 1))))) |#

#| (iter 2 1) |#

#| ... と続くので終わるまで (iter 数値 累積の合計）が呼ばれることとなるので反復的になる |#
