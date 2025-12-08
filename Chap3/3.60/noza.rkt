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

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; 問題 3.59 の級数
(define (integrate-series coeffs)
  (stream-map (lambda (a n) (/ a n))
              coeffs
              integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
               (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

;; 問題 3.60 解答: 級数の乗算
;; 積のイメージ: (a0 + a1x + a2x^2 + …) (b0 + b1x + b2x^2 + …) = (a0b0) + (a0b1 + a1b0)x + (a0b2 + a1b1 + a2b0)x^2 + …
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; 動作確認: sin^2 x + cos^2 x = 1 になることを係数で確認
(define sin2+cos2
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))

(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 先頭 8 項: 1,0,0,... になれば OK
(stream-take sin2+cos2 8)
;; => '(1 0 0 0 0 0 0 0)
