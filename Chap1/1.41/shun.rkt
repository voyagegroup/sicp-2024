#lang racket

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

((double inc) 1)
; (lambda (1) (inc (inc 1)))
; (inc (+ 1 1))
; (inc 2)
; (+ 2 1)
; 3

(((double (double double)) inc) 5)
(((lambda (x) ((double double) ((double double) x)) inc)) 5)
; lambdaのxにincが入る
(((double double) ((double double) inc)) 5)
(((lambda (x) (double (double x))) ((lambda (x) (double (double x))) inc)) 5)
(((lambda (x) (lambda (y) (double x) ((double x) y))) ((lambda (x) (double (double x))) inc)) 5)
(((lambda (x) (lambda (y) (lambda (z) (x (x z))) ((lambda (z) (x (x z))) y))) ((lambda (x) (double (double x))) inc)) 5)
; zに((lambda (z) (x (x z))) y)を代入
(((lambda (x) (lambda (y) (x (x ((lambda (z) (x (x z))) y))) )) ((lambda (x) (double (double x))) inc)) 5)
; zにyを代入
(((lambda (x) (lambda (y) (x (x ((x (x y)))) ))) ((lambda (x) (double (double x))) inc)) 5)
; 同様の流れを右の(lambda (x) (double (double x)))に対しても適用
(((lambda (x) (lambda (y) (x (x ((x (x y))))) )) ((lambda (x) (lambda (y) (x (x (x (x y)))) )) inc)) 5)
; incをyに代入
(((lambda (x) (lambda (y) (x (x ((x (x y)))) ))) (lambda (y) (inc (inc (inc (inc y)))))) 5)
; xに(lambda (y) (inc (inc (inc (inc y)))))を代入
(((lambda (y) ((lambda (y) (inc (inc (inc (inc y))))) ((lambda (y) (inc (inc (inc (inc y))))) (((lambda (y) (inc (inc (inc (inc y))))) ((lambda (y) (inc (inc (inc (inc y))))) y)))) )))  5)
; yにyを代入
(((lambda (y) ((lambda (y) (inc (inc (inc (inc y))))) ((lambda (y) (inc (inc (inc (inc y))))) (((lambda (y) (inc (inc (inc (inc y))))) (inc (inc (inc (inc y))))))) )))  5)
; yに(inc (inc (inc (inc y))))を代入
(((lambda (y) ((lambda (y) (inc (inc (inc (inc y))))) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc y))))))) ))))) )))  5)
; yに(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc y))))))) )))))を代入
((lambda (y) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc y))))))) ))))))))))  5)
; yに5を代入
((lambda (5) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))) ))))))))))  5)
; incが16回実行されて21が出る

; (((double (double double)) inc) n)
; 2の2^2乗回incが実行される