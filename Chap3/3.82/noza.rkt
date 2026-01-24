#lang racket

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

(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

(define (square x) (* x x))

;; 3.5 の random-in-range を流用
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

;; 乱数ストリーム
(define (random-stream)
  (cons-stream (random)
               (random-stream)))

;; monte-carlo: 実験ストリームから確率見積りストリームを作る
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral P x1 x2 y1 y2)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (define experiment-stream
    (cons-stream (experiment)
                 (stream-map (lambda (_) (experiment))
                             (random-stream))))
  (let ((rect-area (* (- x2 x1) (- y2 y1))))
    (stream-map (lambda (p) (* rect-area p))
                (monte-carlo experiment-stream 0 0))))

;; 例: πの推定
(define (estimate-pi)
  (define (in-unit-circle? x y)
    (<= (+ (square x) (square y)) 1))
  (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0))

(stream-take (estimate-pi) 10)
