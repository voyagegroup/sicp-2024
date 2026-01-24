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

;; デバッグ用: 先頭から k 個をリスト化
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

(define random-init 42)

(define (rand-update x)
  (let ((a 1664525)
        (b 1013904223)
        (m 4294967296))
    (modulo (+ (* a x) b) m)))

;; 要求ストリームに従って乱数列を生成する
;; request: 'generate または (list 'reset new-seed)
(define (random-numbers request-stream)
  (define (next-state state request)
    (cond ((eq? request 'generate)
           (rand-update state))
          ((and (pair? request) (eq? (car request) 'reset))
           (cadr request))
          (else
           (error "Unknown request -- RANDOM-NUMBERS" request))))
  (define states
    (cons-stream random-init
                 (stream-map next-state states request-stream)))
  states)

;; 使用例: request は無限ストリームにする
(define (repeat x)
  (cons-stream x (repeat x)))

(define requests
  (cons-stream 'generate
               (cons-stream 'generate
                            (cons-stream (list 'reset 42)
                                         (cons-stream 'generate
                                                      (repeat 'generate))))))

(stream-take (random-numbers requests) 6)
