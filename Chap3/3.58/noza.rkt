#lang sicp

;; stream の基本
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

;; 先頭から k 個をリスト化するユーティリティ
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 問題の手続き
;; 指定の基数で長除法で1桁ずつ生成するストリーム
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; 動作確認:
;; 1/7 の 10 進展開は 0.142857 で循環する
(define s-1-7 (expand 1 7 10))
(stream-take s-1-7 12)
;; => '(1 4 2 8 5 7 1 4 2 8 5 7)

;; 3/8 の 10 進展開は 0.375 で終わり、以後 0 が続く
(define s-3-8 (expand 3 8 10))
(stream-take s-3-8 8)
;; => '(3 7 5 0 0 0 0 0)
