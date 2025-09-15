#lang sicp

(define (has-eq? x li)
    (if (null? li)
        #f
        (if (eq? x (car li))
            #t
            (has-eq? x (cdr li)))))

(define (has-cycle? li)
  (define counted '())
  (define (has-cycle-itr x)
    (if (null? x)
        #f
        (if (has-eq? x counted)
            #t
            (begin
              (set! counted (cons x counted))
              (has-cycle-itr (cdr x))))))
  (has-cycle-itr li))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(has-cycle? z)
; #t

(has-cycle? (list 'a 'b 'c))
; #f