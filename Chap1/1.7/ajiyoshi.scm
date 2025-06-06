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

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess))
	 (abs (* 0.001 guess))))

; (sqrt 10000000000000)
; (sqrt 0.0001)
