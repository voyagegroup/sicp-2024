(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001)) ; 立方根なのでここが3乗になる

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)) ; 近似を求める

(define (my-sqrt x)
  (sqrt-iter 1 x))

; 検算
(my-sqrt 8)
(my-sqrt 27)
(my-sqrt 64)
;(my-sqrt 1000) ; 少し待ったけど計算が終わらなかった