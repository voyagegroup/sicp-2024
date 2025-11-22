#lang sicp

;; stream を作成するためのマクロ
;; AI に作ってもらいました
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define the-empty-stream '())
(define (stream-null? s) (null? s))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))
))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (show x)
  (display x)
  (newline)
  x)

(define (do-nothing x) x)

;; 解答
(define x (stream-map show (stream-enumerate-interval 0 10)))
;;(define x (stream-map do-nothing (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(display "---")
(newline)
(stream-ref x 7)
