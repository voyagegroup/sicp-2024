(define (sqrt-iter guess x)
  (display guess)
  (newline)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(define (square x) (* x x))

;; ここまで本の内容の実装

;(my-sqrt 0.0000000001) ; A. 0.000001
;(my-sqrt 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000) ; 途中から同じ値がguessとなり止まらない


(define (new-good-enough? guess next-guess)
  (< (/ (abs (- next-guess guess)) next-guess) 0.001))

(define (new-sqrt-iter guess x)
  (if (new-good-enough? guess (improve guess x))
      guess
      (new-sqrt-iter (improve guess x) x)))

(define (new-my-sqrt x)
  (new-sqrt-iter 1.0 x))

(new-my-sqrt 0.0000000001)
(new-my-sqrt 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

; どっちも動いた