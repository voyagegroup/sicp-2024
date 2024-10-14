;
#lang sicp

(define (pascal n k)
  (cond ((= k 0) 1)
        ((= k n) 1)
        (else (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k)))))
(pascal 4 0); 1
(pascal 4 1); 4
(pascal 4 2); 6
(pascal 4 3); 4
(pascal 4 4); 1

