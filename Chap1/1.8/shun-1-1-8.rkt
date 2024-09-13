#lang racket

(define (approximation guess x)
(/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess pre-guess)
(< (abs (- guess pre-guess)) 0.001))

(define (cube-iter guess pre-guess x)
(if (good-enough? guess pre-guess)
guess
(cube-iter (approximation guess x) guess x)))

(define (cube x)
(cube-iter 1.0 0 x))
