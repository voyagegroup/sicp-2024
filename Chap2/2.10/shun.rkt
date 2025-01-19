#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((< xu 0)
           (cond ((< yu 0) (make-interval (* xu yu) (* xl yl))) ; x(-3 -1) y(-6 -5)
                 ((< yl 0) (make-interval (* xl yu) (* xl yl))) ; x(-3 -1) y(-3 1)
                 (else (make-interval (* xl yu) (* xu yl))))) ; x(-3 -1) y(2 4)
          ((< xl 0)
           (cond ((< yu 0) (make-interval (* xu yl) (* xl yl))) ; x(-3 1) y(-4 -2)
                 ((< yl 0) (make-interval (min (* xl yu) (* xu yl)) (max (* xl yl)))) ; x(-1 1) y(-1 4), x(-3 1) y(-4 2)
                 (else (make-interval (* xl yu) (* xu yu))))) ; x(-3 1) y(2 4)
          (else
           (cond ((< yu 0) (make-interval (* xu yl) (* xu yl))) ; x(1 3) y(-4 -2)
                 ((< yl 0) (make-interval (* xu yl) (* xu yu))) ; x(1 3) y(-4 2)
                 (else (make-interval (* xl yl) (* xu yu)))) ; x(1 3) y(2 4)
           ))))

(define (div-interval x y)
  (let ((yl (lower-bound y))
        (yu (upper-bound y))
        )
  (if (and (< yl 0) (< yu 0))
      (error "error")
      (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
  ))