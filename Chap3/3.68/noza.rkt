#lang sicp

;; stream 基本
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

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;; Louis の提案版 (本文の式)。第二引数が評価される時点で pairs が即座に再帰し続ける。
;; (pairs integers integers) を評価すると、cons-stream による遅延の前に
;; (pairs (stream-cdr s) (stream-cdr t)) が無限に展開されて停止しない。
(define (pairs-bad s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (pairs-bad (stream-cdr s) (stream-cdr t))))
