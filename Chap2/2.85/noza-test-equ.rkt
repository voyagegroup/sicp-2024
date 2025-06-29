#|
2.79 の実装をしていなかったので、ここで実装する
|#
#lang racket

(require "noza.rkt")

;; Test equ? for scheme-number
(displayln "Testing scheme-number:")
(displayln (equ? (make-scheme-number 5) (make-scheme-number 5))) ; #t
(displayln (equ? (make-scheme-number 5) (make-scheme-number 6))) ; #f

;; Test equ? for rational
(displayln "\nTesting rational:")
(displayln (equ? (make-rational 1 2) (make-rational 1 2))) ; #t
(displayln (equ? (make-rational 2 4) (make-rational 1 2))) ; #t (simplified)
(displayln (equ? (make-rational 1 2) (make-rational 1 3))) ; #f

;; Test equ? for complex
(displayln "\nTesting complex:")
(displayln (equ? (make-complex-from-real-imag 3 4)
                 (make-complex-from-real-imag 3 4))) ; #t
(displayln (equ? (make-complex-from-real-imag 3 4)
                 (make-complex-from-real-imag 3 5))) ; #f
(displayln (equ? (make-complex-from-real-imag 3 4)
                 (make-complex-from-real-imag 4 4))) ; #f

;; Test equ? across different types (with type coercion)
(displayln "\nTesting across types:")
(displayln (equ? (make-scheme-number 5) (make-rational 5 1))) ; #t
(displayln (equ? (make-scheme-number 5) (make-rational 10 2))) ; #t
(displayln (equ? (make-scheme-number 5) (make-complex-from-real-imag 5 0))) ; #t
(displayln (equ? (make-rational 1 2) (make-complex-from-real-imag 0.5 0))) ; #t
