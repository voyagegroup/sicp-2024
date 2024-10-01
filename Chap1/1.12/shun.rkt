#lang racket

(define (pascal x y)
  (cond ((= x 1) 1); 三角形の左端
        ((= x y) 1); 三角形の右端
        (else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))));自分の上の段の値を足す
        )
  )

(pascal 3 5)