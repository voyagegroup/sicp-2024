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

;; 一般的な二階微分方程式: d2y/dt2 = f(dy/dt, y)
(define (solve-2nd f y0 dy0 dt)
  (let ((y 'unassigned)
        (dy 'unassigned)
        (ddy 'unassigned))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (stream-map f dy y))
    y))

;; 3.78 の線形ケースを 3.79 で再現
(define (solve-2nd-linear a b y0 dy0 dt)
  (solve-2nd (lambda (dy y) (+ (* a dy) (* b y))) y0 dy0 dt))

;; 例: y'' = -y, y(0)=0, y'(0)=1 (sin)
(stream-ref (solve-2nd-linear 0 -1 0 1 0.001) 1000)
