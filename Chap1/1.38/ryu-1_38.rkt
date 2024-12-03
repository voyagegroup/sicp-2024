#lang sicp


(define (cont-frac n d k)
  (define (cont-iter i)
    (if (< k i)
        0
        (/ (n i) (+ (d i) (cont-iter (+ i 1))))))
  (cont-iter 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)


; e = 2.718281828459...

(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (remainder i 3) 2)
                 (/ (* (+ i 1) 2) 3)
                 1))
           100)
; -> 0.7182818284590453

; e - 2 なので、+2しないといけない

(define (euler k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (remainder i 3) 2)
                 (/ (* (+ i 1) 2) 3)
                 1))
           k)
     ))

(euler 100)
; 2.7182818284590455
  

