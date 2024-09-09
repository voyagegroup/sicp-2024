(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))
;; ここまで本の定義

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(my-sqrt 3)
;-> (sqrt-iter 1.0 3)
;-> (new-if (good-enough? 1.0 3)
;            3
;            (sqrt-iter (improve 1 3) 3)))
;-> (new-if (good-enough? 1.0 3)
;            3
;            (sqrt-iter (average 1.0 (/ 3 1)))))
;-> (new-if (good-enough? 1.0 3)
;            3
;            (sqrt-iter (average 1.0 3))))
;-> (new-if (good-enough? 1.0 3)
;            3
;            (sqrt-iter (/ (+ 1.0 3) 2))))
;-> (new-if (good-enough? 1.0 3)
;            3
;            (sqrt-iter 2)))
;-> (new-if (good-enough? 1.0 3)
;            3
;            (new-if (good-enough? 2 1)
;                     2
;                     (sqrt-iter (improve 2 3) 3)))
;
;; となり、new-ifの引数であるsqrt-iterの評価が先に行われ関数を取り出した際に再び次の予測値を用いたnew-ifが始まりまたsqrt-iterの評価が行われ更に次のnew-ifからの評価が行われるため無限ループとなる
             
