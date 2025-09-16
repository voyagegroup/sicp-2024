#lang sicp
(define (count-pairs x)
  (let ((counted '()))  ; 既に数えた対のリスト
    (define (count-helper x)
      (cond ((not (pair? x)) 0)
            ((memq x counted) 0)  ; 既に数えた対なら0を返す
            (else
             (set! counted (cons x counted))  ; 対を記録
             (+ (count-helper (car x))
                (count-helper (cdr x))
                1))))
    (count-helper x)))


(define x (list 'a 'b))
(define z1 (cons x x))

z1

(define z2 (cons (list 'a 'b) (list 'a 'b)))
z2

(count-pairs z1)
; -> 3
; memo: もともと、5が返っていた
(count-pairs z2)
; -> 5


(define l4-3 (cons 'a '()))
(define l4-2 (cons 'b l4-3))
(define l4-1 (cons l4-3 l4-2))
l4-1
(count-pairs l4-1)
; -> 3
; memo: もともと、4が返っていた

(define l7-3 (cons 'a '()))
(define l7-2 (cons l7-3 l7-3))
(define l7-1 (cons l7-2 l7-2))
l7-1
(count-pairs l7-1)
; -3
; memo: もともと7


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z-cycle (make-cycle (list 'a 'b 'c)))

(count-pairs z-cycle)
; -> 3
; memo: もともと、ループ

