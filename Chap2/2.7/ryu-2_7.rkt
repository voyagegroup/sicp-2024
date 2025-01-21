#lang sicp


(define (make-interval a b) (cons a b))

; -- upper と lower を実装 --
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; -- 動作確認 --

(define interval (make-interval 6.12 7.48))

(upper-bound interval)
; -> 7.48
(lower-bound interval)
; -> 6.12
