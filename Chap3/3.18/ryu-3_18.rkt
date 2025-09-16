#lang sicp


(define (cycle? x)
  (let ((seen '()))
    (define (check x)
      (cond ((null? x) #f) ; 終端に到達 -> ループなし
            ((memq x seen) #t) ; 既に見た -> ループあり
            (else
             (set! seen (cons x seen))
             (check (cdr x)))))
    (check x)))


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z-cycle (make-cycle (list 'a 'b 'c)))

(cycle? z-cycle)
; -> #t


(define x (list 'a 'b))
(define z1 (cons x x))

z1

(cycle? z1)
; -> #f