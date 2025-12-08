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

;; 問題 3.59 解答
;; a: 級数 a0 + a1 x + a2 x^2 + ... を
;;    a0/1, a1/2, a2/3, ... に変換する（積分定数は別扱い）。
(define (integrate-series coeffs)
  (stream-map (lambda (a n) (/ a n))
              coeffs
              integers))

;; b: exp, sin, cos の級数を相互再帰で生成
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
               (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

;; 動作確認用: 先頭 k 個をリスト化
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; それぞれの級数の先頭係数を確認
(stream-take exp-series 6)
;; => '(1 1 1/2 1/6 1/24 1/120)

(stream-take sine-series 7)
;; => '(0 1 0 -1/6 0 1/120 0)

(stream-take cosine-series 7)
;; => '(1 0 -1/2 0 1/24 0 -1/720)
