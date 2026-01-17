#lang sicp

;; stream 基本
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define the-empty-stream '())
(define (stream-null? s) (null? s))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

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

;; デバッグ用: 先頭から k 個をリスト化
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 積分器: integrand を遅延引数として受け取る版
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; 直列RLC回路モデル
;; R: 抵抗値, L: インダクタ, C: キャパシタ, dt: サンプリング間隔
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (let ((vC 'unassigned)
          (iL 'unassigned)
          (dvC 'unassigned)
          (diL 'unassigned))
      (set! vC (integral (delay dvC) vC0 dt))
      (set! iL (integral (delay diL) iL0 dt))
      (set! dvC (scale-stream iL (/ 1.0 C)))
      (set! diL (add-streams (scale-stream iL (- (/ R L)))
                             (scale-stream vC (- (/ 1.0 L)))))
      (cons vC iL))))

;; 確認用: R=1, C=0.2, L=1, dt=0.1, iL0=0, vC0=10
(define rlc1 (RLC 1 1 0.2 0.1))
(define rlc-streams (rlc1 10 0))
(define vC (car rlc-streams))
(define iL (cdr rlc-streams))

(stream-take vC 10)
(stream-take iL 10)
