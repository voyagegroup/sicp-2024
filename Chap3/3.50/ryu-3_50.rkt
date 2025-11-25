#lang sicp

#|
2.2.1
(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
(741 852 963)

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))
(9 12 15)

cons-stream: 構成子
apply:
(define nums (list 1 2 3))
(apply + nums)
6

|#


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


(define s1 (stream 1 2 3))
(define s2 (stream 10 20 30))
(stream-map + s1 s2)

; (stream-null? s1) -> #f

;(cons-stream
; (apply + (map stream-car (list s1 s2))) ; ①
; (apply stream-map
;        (cons + (map stream-cdr (list s1 s2))))) ; ②

; ①
; (apply + (map stream-car (list s1 s2)))
; (stream-car s1) -> 1
; (stream-car s2) -> 10
; (apply + (list 1 10))
; -> 11

; ②
; (apply stream-map
;        (cons + (map stream-cdr (list s1 s2))))
; (stream-cdr s1) -> (2 3)
; (stream-cdr s2) -> (20 30)
; (stream-map + (stream 2 3) (stream 20 30))
