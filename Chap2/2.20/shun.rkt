#lang racket

(define (is-even n)
  (= (remainder n 2) 0))

(define (same-parity a . d)
  (define (make-e-li resu li)
    (if (null? li)
        resu
        (if (is-even (car li))
            (make-e-li (append resu (list (car li))) (cdr li))
            (make-e-li resu (cdr li)))))
   (define (make-o-li resu li)
    (if (null? li)
        resu
        (if (is-even (car li))
            (make-o-li resu (cdr li))
            (make-o-li (append resu (list (car li))) (cdr li)))))
  (if (is-even a)
      (make-e-li (list a) d)
      (make-o-li (list a) d)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)