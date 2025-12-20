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

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 部分和のストリーム
(define (partial-sums s)
  (define sum (stream-car s))
  (cons-stream sum
               (add-streams (stream-cdr s)
                            (partial-sums s))))

(define (square x) (* x x))

;; Euler 変換とタブローによる加速
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;; ln2 = 1 - 1/2 + 1/3 - 1/4 + ...
(define (ln2-summands n)
  (let ((term (/ 1.0 n)))
    (cons-stream (if (even? n) (- term) term)
                 (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define ln2-euler
  (euler-transform ln2-stream))

(define ln2-super
  (accelerated-sequence euler-transform ln2-stream))

;; 動作確認: 先頭 8 項の収束の比較
(define sample-count 8)
(list 'raw (stream-take ln2-stream sample-count)
      'euler (stream-take ln2-euler sample-count)
      'accelerated (stream-take ln2-super sample-count))
