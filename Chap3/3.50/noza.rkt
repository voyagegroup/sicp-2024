#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define (stream-null? s) (null? s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

;; 解答
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; 動作確認用
;; stream を作成するためのマクロ
;; AI に作ってもらいました
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

;; ストリームから指定した要素数のリストを作成する
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 動作確認
(stream-take (stream-map (lambda (x) (* x x))
                         (stream-enumerate-interval 1 5))
             5)
;; => '(1 4 9 16 25)
