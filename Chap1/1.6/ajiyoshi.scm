(define (new-if pred then-clause else-clause)
	(cond (pred then-clause)
		  (else else-clause)))

(define (new-sqrt-iter guess x)
	(new-if (good-enough? guess x)
			guess
			(new-sqrt-iter (improve guess x) x)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))


(new-sqrt-iter 1.0 2.0)

; -> 仮パラメタを対応する引数で取り替え、手続きの本体を評価する
; (new-if (good-enough? guess x)
;		 guess
;		 (new-sqrt-iter (improve guess x) x))
(new-if (good-enough? 1.0 2.0)
		1.0
		(new-sqrt-iter (improve 1.0 2.0) 2.0))
; 組み合わせの部分式を評価する
; (good-enough? 1.0 2.0)
; -> 仮パラメタを対応する引数で取り替え、手続きの本体を評価する
; (< (abs (- (square guess) x)) 0.001)

(new-if (< (abs (- (square 1.0) 2.0)) 0.001)
		1.0
		(new-sqrt-iter (improve 1.0 2.0) 2.0))
; 組み合わせの部分式を評価する
; -> 仮パラメタを対応する引数で取り替え、手続きの本体を評価する
; (square 1.0) -> (* x x) -> (* 1.0 1.0) -> 1.0
(new-if (< (abs (- 1.0 2.0)) 0.001)
		1.0
		(new-sqrt-iter (improve 1.0 2.0) 2.0))
; 組み合わせの部分式を評価する
; (- 1.0 2.0) -> -1
(new-if (< (abs -1.0) 0.001)
		1.0
		(new-sqrt-iter (improve 1.0 2.0) 2.0))
; 組み合わせの部分式を評価する
; (abs -1.0) -> 1.0
(new-if (< 1.0 0.001)
		1.0
		(new-sqrt-iter (improve 1.0 2.0) 2.0))
; ->
(new-if #f
		1.0
		(new-sqrt-iter (improve 1.0 2.0) 2.0))

; 組み合わせの部分式を評価する
;(new-sqrt-iter (improve 1.0 2.0) 2.0)
(new-if #f
		1.0
		(new-sqrt-iter (improve 1.0 2.0) 2.0))
; 組み合わせの部分式を評価する
; (improve 1.0 2.0)
; -> 仮パラメタを対応する引数で取り替え、手続きの本体を評価する
; -> (average guess (/ x guess)) -> (average 1.0 (/ 2.0 1.0))
; 組み合わせの部分式を評価する
; -> (average 1.0 2.0)
; -> 仮パラメタを対応する引数で取り替え、手続きの本体を評価する
; -> (/ (+ x y) 2) -> (/ (+ 1.0 2.0) 2) -> (/ 3.0 2) -> 1.5
(new-if #f
		1.0
		(new-sqrt-iter 1.5 2.0))
; -> 仮パラメタを対応する引数で取り替え、手続きの本体を評価する
; (new-sqrt-iter 1.5 2.0)
; -> (new-if (good-enough? 1.5 2.0)
;		 1.5
;		 (new-sqrt-iter (improve 1.5 2.0) 2.0))
(new-if #f
		1.0
		(new-if (good-enough? 1.5 2.0)
				1.5
				(new-sqrt-iter (improve 1.5 2.0) 2.0)))
; (good-enough? 1.5 2.0) -> #f
; 組み合わせの部分式を評価する
; (new-sqrt-iter (improve 1.5 2.0) 2.0)
; 組み合わせの部分式を評価する
; (improve 1.5 2.0) ->
; -> (average 1.5 (/ 2.0 1.5)) -> (average 1.5 (/ 2.0 1.5))
; -> (average 1.5 1.333)
; -> (/ (+ 1.5 1.333) 2) -> (/ 2.8332) -> 1.416
(new-if #f
		1.0
		(new-if #f
				1.5
				(new-sqrt-iter 1.416 2.0)))
; 組み合わせの部分式を評価する
; (new-sqrt-iter 1.416 2.0)
;
(new-if #f
		1.0
		(new-if #f
				1.5
				(new-sqrt-iter 1.416 2.0)))
;(new-sqrt-iter 1.416 2.0)
; ->
;(new-if (good-enough? 1.416 2.0)
;		 1.416
;		 (new-sqrt-iter (improve 1.416 2.0) 2.0))
(new-if #f
		1.0
		(new-if #f
				1.5
				(new-if (good-enough? 1.416 2.0)
						1.416
						(new-sqrt-iter (improve 1.416 2.0) 2.0))))
; 組み合わせの部分式を評価する
; (good-enough? 1.416 2.0) -> #t
(new-if #f
		1.0
		(new-if #f
				1.5
				(new-if #t
						1.416
						(new-sqrt-iter (improve 1.416 2.0) 2.0))))
; 組み合わせの部分式を評価する
; (new-sqrt-iter (improve 1.416 2.0) 2.0)
; ->
; (new-if (good-enough? 1.41421 2.0)
;		 1.41421
;		 (new-sqrt-iter (improve 1.41421 2.0) 2.0))
(new-if #f
		1.0
		(new-if #f
				1.5
				(new-if #t
						1.416
						(new-if (good-enough? 1.41421 2.0)
								1.41421
								(new-sqrt-iter (improve 1.41421 2.0) 2.0)))))
; 組み合わせの部分式を評価する
; (good-enough? 1.416 2.0) -> #t
(new-if #f
		1.0
		(new-if #f
				1.5
				(new-if #t
						1.416
						(new-if #t
								1.41421
								(new-sqrt-iter (improve 1.41421 2.0) 2.0)))))
; 組み合わせの部分式を評価する
; (new-sqrt-iter (improve 1.416 2.0) 2.0)
; ->
; (new-if (good-enough? 1.416 2.0)
(new-if #f
		1.0
		(new-if #f
				1.5
				(new-if #t
						1.416
						(new-if #t
								1.41421
								(new-if (good-enough? 1.41421 2.0)
										1.41421
										(new-sqrt-iter (improve 1.41421 2.0) 2.0))))))
