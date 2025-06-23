#lang racket

(require "noza.rkt")

;; テスト用のデータ
(define s10 (make-scheme-number 10))
(define s5 (make-scheme-number 5))
(define s2 (make-scheme-number 2))

(define r1/2 (make-rational 1 2))
(define r1/4 (make-rational 1 4))

(display "Testing left-to-right evaluation with 3 or more arguments:\n\n")

(display "Subtraction tests:\n")
(display "sub 10 5 2 = ") (display (sub s10 s5 s2)) (display " (should be 3)\n")
(display "sub 10 1/2 1/4 = ") (display (sub s10 r1/2 r1/4)) (display " (should be 37/4)\n")
(newline)

(display "Division tests:\n")
(display "div 10 5 2 = ") (display (div s10 s5 s2)) (display " (should be 1)\n")
(display "div 10 1/2 2 = ") (display (div s10 r1/2 s2)) (display " (should be 10 . 1)\n")
(newline)

(display "Addition tests (for comparison):\n")
(display "add 10 5 2 = ") (display (add s10 s5 s2)) (display " (should be 17)\n")
(display "add 10 1/2 1/4 = ") (display (add s10 r1/2 r1/4)) (display " (should be 43/4)\n")
(newline)

(display "Multiplication tests (for comparison):\n")
(display "mul 10 5 2 = ") (display (mul s10 s5 s2)) (display " (should be 100)\n")
(display "mul 10 1/2 2 = ") (display (mul s10 r1/2 s2)) (display " (should be 10)\n")
