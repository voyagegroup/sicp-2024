#lang sicp

;; stream を作成するためのマクロ
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define the-empty-stream '())
(define (stream-null? s) (null? s))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; partial-sums: 累積和をストリームで返す
(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (stream-cdr s)
                (partial-sums s))))

;; 動作確認: 1, 3, 6, 10, 15, 21, ...
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

(stream-take (partial-sums integers) 7)
;; => '(1 3 6 10 15 21 28)
