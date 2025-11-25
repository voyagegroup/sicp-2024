#lang sicp

; --- 3.5.1

(define (display-line x)
  (newline)
  (display x))


; (stream-car (cons-stream x y)) = x
(define (stream-car stream) (car stream))
; (stream-cdr (cons-stream x y)) = y
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; --- 3.50


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; --- 3.51
(define (show x)
  (display-line x)
  x)


(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
(stream-ref x 10)
(stream-ref x 6)

(define y (stream-map odd? (stream-enumerate-interval 0 10)))
(stream-ref y 5)
(stream-ref y 6)
