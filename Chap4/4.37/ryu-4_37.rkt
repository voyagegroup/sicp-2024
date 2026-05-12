#lang sicp

(define (require p)
  (if (not p) (amb)))

; 二つの与えられた限界の間の整数を返す手続き
(define (an-integer-between s e)
  (require (<= s e))
  (amb s (an-integer-between (+ s 1) e)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (display 'hsq:)
    (display hsq) (newline)
    (display 'i:)
    (display i)(newline)
    (let ((j (an-integer-between i high)))
      (display 'j)
      (display j)(newline)
      (let ((ksq (+ (* i i) (* j j))))
        (display 'ksq)
        (display ksq) (newline)
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (display 'k:)
          (display k)(newline)
          (require (integer? k))
          (list i j k))))))

(a-pythagorean-triple-between 1 5)
#|
hsq:25
i:1
j1
ksq2
k:1.4142135623730951
j2
ksq5
k:2.23606797749979
j3
ksq10
k:3.1622776601683795
j4
ksq17
k:4.123105625617661
j5
ksq26
hsq:25
i:2
j2
ksq8
k:2.8284271247461903
j3
ksq13
k:3.605551275463989
j4
ksq20
k:4.47213595499958
j5
ksq29
hsq:25
i:3
j3
ksq18
k:4.242640687119285
j4
ksq25
k:5
(3 4 5)
|#


; Q. これは正しいか
; A. 正しい。元が、O(n^3)だったのが、O(n^2)に改善している
;  (ヒント: 調べなければならない可能性の数を考えよ.)
; ambがどれだけ分岐するか。今回の場合、kが分岐しなくなった