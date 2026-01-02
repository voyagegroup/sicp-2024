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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;; 積分器: 初期値 initial-value, 入力 integrand を dt で積分
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; RC 回路モデル
;; R: 抵抗値, C: キャパシタ, dt: サンプリング間隔
(define (RC R C dt)
  (lambda (i-stream v0)
    (let* ((q0 (* C v0))                                  ; 初期電荷
           (charge (integral i-stream q0 dt))             ; 電荷ストリーム
           (vc (scale-stream charge (/ 1.0 C)))           ; キャパシタ電圧
           (vr (scale-stream i-stream R)))                ; 抵抗電圧
      (add-streams vr vc))) )                             ; 合成電圧

;; 確認用
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; サンプル: R=5, C=1, dt=0.5 で定電流 1A を流す
(define (ones) (cons-stream 1 (ones)))
(define rc1 (RC 5 1 0.5))
(define v1 (rc1 (ones) 0)) ; 初期電圧 0V のときの応答

(stream-take v1 10)
