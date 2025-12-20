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

;; 問題 3.64: 連続する 2 項の差が tolerance 未満になったら、その後者を返す
(define (stream-limit s tolerance)
  (define (iter prev rest)
    (let ((next (stream-car rest)))
      (if (< (abs (- next prev)) tolerance)
          next
          (iter next (stream-cdr rest)))))
  (iter (stream-car s) (stream-cdr s)))

;; 以下は動作確認用: 3.63 の sqrt-stream
(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; サンプル: √2 を 1e-6 の誤差で
(sqrt 2 1e-6)
