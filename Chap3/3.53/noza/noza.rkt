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

;; 先頭から k 個をリスト化して確認するためのユーティリティ
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 解答: s = 1, 2, 4, 8, 16, ... (2 のべき乗のストリーム)
(define s (cons-stream 1 (add-streams s s)))

;; 動作確認: 先頭 10 要素を取り出す
(stream-take s 10)
;; => '(1 2 4 8 16 32 64 128 256 512)
