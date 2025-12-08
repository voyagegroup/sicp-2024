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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; 3.59 より: 級数の積分
(define (integrate-series coeffs)
  (stream-map (lambda (a n) (/ a n))
              coeffs
              integers))

;; 3.60 より: 級数の乗算（畳み込み）
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; 3.61 解答: 定数項が 1 の級数 S の逆数 1/S
(define (invert-unit-series s)
  (let ((sr (stream-cdr s))) ; S = 1 + SR
    (define inv
      (cons-stream 1
                   (scale-stream (mul-series sr inv) -1)))
    inv))

;; 動作確認: exp-series の逆数は e^{-x} の級数になるはず
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define inv-exp-series (invert-unit-series exp-series))

;; 確認: exp-series * inv-exp-series = 1 になることを先頭項でチェック
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

(define prod-check (mul-series exp-series inv-exp-series))
(stream-take prod-check 6)
;; => '(1 0 0 0 0 0)
