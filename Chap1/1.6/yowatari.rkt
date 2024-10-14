;
#lang sicp

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x)
  (* x x))
(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(my-sqrt 9)
; 3.00009155413138
(my-sqrt (+ 100 37))
; 11.704699917758145
(my-sqrt (+ (my-sqrt 2) (my-sqrt 3)))
; 1.7739279023207892
(square (my-sqrt 1000))
; 1000.000369924366

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(new-if (= 2 3) 0 5)
; 5
(new-if (= 1 1) 0 5)
; 0

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                     x)))
;(define (new-my-sqrt x)
;  (new-sqrt-iter 1.0 x))

;(new-my-sqrt 9)
;(new-sqrt-iter 1.0 9)
;(new-if (good-enough? 1.0 9)
;        1.0
;        (new-sqrt-iter (improve 1.0 9)
;                       9))
;(new-if (< (abs (- (square 1.0) 9)) 0.001)
;        1.0
;        (new-sqrt-iter (improve 1.0 9)
;                       9))
;(new-if (< (abs (- 1.0 9)) 0.001)
;        1.0
;        (new-sqrt-iter (improve 1.0 9)
;                       9))
;(new-if (< (abs -8.0) 0.001)
;        1.0
;        (new-sqrt-iter (improve 1.0 9)
;                       9))
;(new-if (< 8.0 0.001)
;        1.0
;        (new-sqrt-iter (improve 1.0 9)
;                       9))
;(new-if #f
;        1.0
;        (new-sqrt-iter (improve 1.0 9)
;                       9))
;(new-sqrt-iter (improve 1.0 9)
;               9))
;(new-sqrt-iter (average 1.0 (/ 9 1.0))
;               9))
;(new-sqrt-iter (average 1.0 9)
;               9))
;(new-sqrt-iter 5
;               9))
;(new-if #f
;        1.0
;        (new-sqrt-iter 5 9))
;(new-if #f
;        1.0
;        (new-if (good-enough? 5 9)
;                5
;                (new-sqrt-iter (improve 5 9)
;                               9)))
;(good-enough? 5 9) => (< (abs (- (square 5) 9)) 0.001) => #f
;(improve 5 9) => (average 5 (/ 9 5)) => 3.4
;(new-if #f
;        1.0
;        (new-if #f
;                5
;                (new-sqrt-iter 3.4 9)))
;(new-if #f
;        1.0
;        (new-if #f
;                5
;                (new-if (good-enough? 3.4 9)
;                        3.4
;                        (new-sqrt-iter (improve 3.4 9)
;                                       9))))
;(good-enough? 3.4 9) => (< (abs (- (square 3.4) 9)) 0.001) => #f
;(improve 3.4 9) => (average 3.4 (/ 9 3.4)) => 3.0235294118
;(new-if #f
;        1.0
;        (new-if #f
;                5
;                (new-if #f
;                        3.4
;                        (new-sqrt-iter 3.0235294118
;                                       9))))
;(new-if #f
;        1.0
;        (new-if #f
;                5
;                (new-if #f
;                        3.4
;                        (new-if (good-enough? 3.0235294118 9)
;                                3.0235294118
;                                (new-sqrt-iter (improve 3.0235294118 9)
;                                               9)))))
;(good-enough? 3.0235294118 9) => (< (abs (- (square 3.0235294118) 9)) 0.001) => #f
;(improve 3.0235294118 9) => (average 3.0235294118 (/ 9 3.0235294118) => 3.0000915541
;(new-if #f
;        1.0
;        (new-if #f
;                5
;                (new-if #f
;                        3.4
;                        (new-if #f
;                                3.0235294118
;                                (new-sqrt-iter 3.0000915541
;                                               9)))))
;(new-if #f
;        1.0
;        (new-if #f
;                5
;                (new-if #f
;                        3.4
;                        (new-if #f
;                                3.0235294118
;                                (new-if (good-enough? 3.0000915541 9)
;                                        3.0000915541
;                                        (new-sqrt-iter (improve 3.0000915541 9)
;                                                       9))))))
;(good-enough? 3.0000915541 9) => (< (abs (- (square 3.0000915541) 9)) 0.001 => #t
;(improve 3.0000915541 9) => (average 3.0000915541 (/ 9 3.0000915541)) => 3.0000000014
;(new-if #f
;        1.0
;        (new-if #f
;                5
;                (new-if #f
;                        3.4
;                        (new-if #f
;                                3.0235294118
;                                (new-if #t
;                                        3.0000915541
;                                        (new-sqrt-iter 3.0000000014
;                                                       9))))))
;(new-sqrt-iter 3.0000000014 9) を評価する
;
