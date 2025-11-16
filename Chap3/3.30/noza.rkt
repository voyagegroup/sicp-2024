#lang sicp

(define (ripple-carry-adder a-list b-list s-list c)
  (if (null? (cdr a-list))
      'ok
      (let (new-c (make-wire))
        (full-adder (car a-list) (car blist) c (car s-list) new-c)
        (ripple-carry-adder (cdr a-list) (cdr b-list) (cdr s-list) new-c))))

; full adder について、
;   s: (+ (large or (+ and invert)) and)
;   c: (+ ((+ (large or (+ and invert)) and)) or)
;      or > (+ and invert): (+ (* 2 or) and))
;      or < (+ and invert): (+ (* 2 and) or invert)
;
; (* n (+ (* 2 or) and)) or (* n (+ and invert): (+ (* 2 and) or invert))