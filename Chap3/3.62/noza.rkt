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

;; 3.59: 級数の積分
(define (integrate-series coeffs)
  (stream-map (lambda (a n) (/ a n))
              coeffs
              integers))

;; 3.60: 級数の乗算（畳み込み）
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; 3.61: 定数項が 1 の級数の逆数
(define (invert-unit-series s)
  (let ((sr (stream-cdr s))) ; S = 1 + SR
    (define inv
      (cons-stream 1
                   (scale-stream (mul-series sr inv) -1)))
    inv))

;; 3.62 解答: 一般のべき級数の除算
(define (div-series s1 s2)
  (let ((a0 (stream-car s2)))
    (if (= a0 0)
        (error "div-series: denominator has zero constant term")
        (let* ((unit-den (scale-stream s2 (/ 1 a0)))     ; 先頭を 1 に正規化
               (inv-den (scale-stream                    ; 1/s2 = (1/a0) * 1/unit-den
                        (invert-unit-series unit-den)
                        (/ 1 a0))))
          (mul-series s1 inv-den)))))

;; 3.59 の sin, cos 級数
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
               (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

;; tan x の級数 (sin / cos)
(define tangent-series
  (div-series sine-series cosine-series))

;; 動作確認用: 先頭 k 項をリストに
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 正接の級数の先頭数項を確認
(stream-take tangent-series 6)
;; 期待: 0, 1, 0, 1/3, 0, 2/15, ...
