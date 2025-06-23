#lang racket

(require "noza.rkt")

(displayln "Testing drop procedure:")
(displayln "=======================")

;; Test 1: Complex number with zero imaginary part should drop to scheme-number
(displayln "\nTest 1: Complex 5+0i → should drop to scheme-number 5")
(define c1 (make-complex-from-real-imag 5 0))
(displayln (format "Original: ~a" c1))
(define dropped1 (drop c1))
(displayln (format "Dropped: ~a" dropped1))

;; Test 2: Complex number with non-zero imaginary part should stay complex
(displayln "\nTest 2: Complex 3+4i → should stay complex")
(define c2 (make-complex-from-real-imag 3 4))
(displayln (format "Original: ~a" c2))
(define dropped2 (drop c2))
(displayln (format "Dropped: ~a" dropped2))

;; Test 3: Rational number that equals integer should drop to scheme-number
(displayln "\nTest 3: Rational 6/2 → should drop to scheme-number 3")
(define r1 (make-rational 6 2))
(displayln (format "Original: ~a" r1))
(define dropped3 (drop r1))
(displayln (format "Dropped: ~a" dropped3))

;; Test 4: Rational number that doesn't equal integer should stay rational
(displayln "\nTest 4: Rational 3/2 → should stay rational")
(define r2 (make-rational 3 2))
(displayln (format "Original: ~a" r2))
(define dropped4 (drop r2))
(displayln (format "Dropped: ~a" dropped4))

;; Test 5: Scheme-number should stay scheme-number
(displayln "\nTest 5: Scheme-number 42 → should stay scheme-number")
(define s1 (make-scheme-number 42))
(displayln (format "Original: ~a" s1))
(define dropped5 (drop s1))
(displayln (format "Dropped: ~a" dropped5))

;; Test 6: Verify the drop works correctly through multiple levels
(displayln "\nTest 6: Complex 8+0i → should drop all the way to scheme-number 8")
(define c4 (make-complex-from-real-imag 8 0))
(displayln (format "Original: ~a" c4))
(define dropped7 (drop c4))
(displayln (format "Dropped: ~a" dropped7))
