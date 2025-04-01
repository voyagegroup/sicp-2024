#lang sicp


(equal? '(this is a list) '(this is a list))
; #t

(equal? '(this is a list) '(this (is a) list))
; #f

; --- 挙動をみる ---
(eq? '(this is a list) '(this is a list))
; #f
(eq? '(this is a list) '(this (is a) list))
; #f
(eq? (car '(this is a list)) (car '(this is a list)))
; #t

(equal? 5 '5)
; #t
(eq? 5 '5)
; #t

; --- equal?の実装 ---

(define (my-equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (my-equal? (car a) (car b)) (my-equal? (cdr a) (cdr b))))
        (else #f)))


(my-equal? 1 1)
(my-equal? '(this is a list) '(this is a list))
; #t

(my-equal? '(this is a list) '(this (is a) list))
; #f