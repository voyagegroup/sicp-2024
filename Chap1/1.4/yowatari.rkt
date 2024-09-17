;

#lang sicp

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b (* 1 2) 3)
; (a-plus-abs-b 2 3)
; ((if (> 3 0) + -) 2 3))
; (+ 2 3)
; 10

(a-plus-abs-b (* 5 3) -1)
; (a-plus-abs-b 15 -1)
; ((if (> -1 0) + -) 15 -1))
; (- 15 -1)
; 16

