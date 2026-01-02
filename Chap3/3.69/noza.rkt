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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; ユーティリティ
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

(define (square x) (* x x))

;; 整数ストリーム
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;; i <= j の対を生成
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; i <= j <= k の三つ組を生成
(define (triples s t u)
  (let ((i (stream-car s)))
    (cons-stream
     (list i (stream-car t) (stream-car u))
     (interleave
      (stream-map (lambda (pair) (list i (car pair) (cadr pair)))
                  (stream-cdr (pairs t u))) ; i 固定で j, k を進める
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

;; ピタゴラス三つ組 (i^2 + j^2 = k^2, i <= j)
(define pythagorean-triples
  (stream-filter (lambda (triple)
                   (= (+ (square (car triple))
                         (square (cadr triple)))
                      (square (caddr triple))))
                 (triples integers integers integers)))

;; サンプル取得ヘルパー
(define (take-pythagorean n)
  (stream-take pythagorean-triples n))

(take-pythagorean 5)
