#lang sicp

(define (count-pairs x)
  (define (has-eq? x li)
    (if (null? li)
        #f
        (if (eq? x (car li))
            #t
            (has-eq? x (cdr li)))))
  (define counted '())
  (define (count-pairs-itr x)
    (if (or (not (pair? x)) (has-eq? x counted))
      0
      (begin
        (set! counted (cons x counted))
        (+ (count-pairs-itr (car x))
         (count-pairs-itr (cdr x))
         1))))
  (count-pairs-itr x))

(define a (list 'a))

(count-pairs (list a a))
; 3

(define y (cons 'a 'b))

(define y3 (cons y y))

(define y7 (cons y3 y3))

(count-pairs y7)
; 3