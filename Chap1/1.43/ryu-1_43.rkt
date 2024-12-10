#lang sicp

(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

; ((compose square inc) 6)
; -> 49

; (compose square square) 2)


; memo
; (define (repeated f x)
;   (lambda (n) ((compose f f) n)))
; -> 625
; これで、2が固定なら良い感じになっていそう

(define (repeated f x)
  (if (= x 1)
      f
      (compose f (repeated f (- x 1)))))

((repeated square 1) 5)
; (square 5)
; -> 25

((repeated square 2) 5)
; (repeated square 2)
; (compose square (repeated square 1))
; (compose square square)
; (lambda (x) (square (square x)))
; ----
; ((lambda (x) (square (square x))) 5)
; (square (square 5))
; (square 25)
; -> 625


((repeated square 3) 5)
; (reqeated square 3)
; (compose square (reqeated square 2))
; (compose square (compose square (reqeated square 1)))
; (compose square (compose square square))
; (compose square (labda (x) (square (square x))))
; (lambda (x) (square ((lambda (x) (square (square x))) x)))
; ---
; ((lambda (x) (square ((lambda (x) (square (square x))) x))) 5)
; (square ((lambda (x) (square (square x))) 5))
; (square (square (square 5)))
; -> 390625

