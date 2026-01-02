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

;; ユーティリティ: 先頭から k 個をリスト化
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 符号変化検出: 負→正で +1, 正→負で -1, それ以外 0 （0 の符号は正とみなす）
(define (sign-change-detector current prev)
  (cond ((and (< prev 0) (>= current 0)) 1)
        ((and (>= prev 0) (< current 0)) -1)
        (else 0)))

;; 零交差ストリーム: sense-data と 1 つシフトした過去の値を並行に走査
(define (zero-crossings sense-data)
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data))) ; 初期の過去値は 0 とする

;; 簡易サンプル
(define sample-signal
  (cons-stream 1
               (cons-stream 2
                            (cons-stream 1.5
                                         (cons-stream 1
                                                      (cons-stream 0.5
                                                                   (cons-stream -0.1
                                                                                (cons-stream -2
                                                                                             (cons-stream -3
                                                                                                          (cons-stream -2
                                                                                                                       (cons-stream -0.5
                                                                                                                                    (cons-stream 0.2
                                                                                                                                                 (cons-stream 3
                                                                                                                                                              (cons-stream 4
                                                                                                                                                                           the-empty-stream))))))))))))))

(define sample-zero-crossings
  (zero-crossings sample-signal))

(stream-take sample-zero-crossings 13)
