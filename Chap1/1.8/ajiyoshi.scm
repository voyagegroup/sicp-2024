(define (cuberoot-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (cuberoot-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (/ (abs (- (cube guess) x)) x)
	 0.0001))

(define (improve guess x)
  (/ (+ (/ x (square guess))
		(* 2 guess))
	 3))

(define (cube x)
  (* x x x))

(define (cuberoot x)
  (cuberoot-iter 1.0 x))
