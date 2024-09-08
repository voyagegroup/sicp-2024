(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b (* 2 3) 4)
; -> (a-plus-abs-b 6 4)
; -> ((if (> 4 0) + -) 6 4))
; -> (+ 6 4)
; -> 10

