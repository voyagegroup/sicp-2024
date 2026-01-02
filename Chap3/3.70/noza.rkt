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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; ユーティリティ
(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 整数ストリーム
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;; 重み付きマージ (weight は2引数 i j -> 数値)
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((x1 (stream-car s1))
               (x2 (stream-car s2))
               (w1 (weight (car x1) (cadr x1)))
               (w2 (weight (car x2) (cadr x2))))
           (cond ((< w1 w2)
                  (cons-stream x1
                               (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> w1 w2)
                  (cons-stream x2
                               (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  ;; 同じ重みの場合は両方を順序を保って出力する
                  (cons-stream x1
                               (cons-stream x2
                                            (merge-weighted (stream-cdr s1)
                                                            (stream-cdr s2)
                                                            weight)))))))))

;; 重み付き pairs (i <= j を想定) weight は2引数 i j -> 数値
(define (weighted-pairs s t weight)
  (let ((i0 (stream-car s))
        (t-rest (stream-cdr t)))
    (cons-stream
     (list i0 (stream-car t))
     (merge-weighted
      (stream-map (lambda (x) (list i0 x)) t-rest) ; 同じ i0 の行
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight) ; 右下の行列
      weight))))

;; 3.70a: 和 i + j で順序づけ
(define (weight-sum i j)
  (+ i j))

(define ordered-pairs-by-sum
  (weighted-pairs integers integers weight-sum))

;; 3.70b: 和 2i + 3j + 5ij で順序づけ (i, j は 2,3,5 で割り切れない)
(define (coprime-235? n)
  (and (not (= 0 (remainder n 2)))
       (not (= 0 (remainder n 3)))
       (not (= 0 (remainder n 5)))))

(define integers-235-free
  (stream-filter coprime-235? integers))

(define (weight-235 i j)
  (+ (* 2 i)
     (* 3 j)
     (* 5 i j)))

(define ordered-pairs-235
  (weighted-pairs integers-235-free integers-235-free weight-235))

;; サンプル取得ヘルパー
(define (take-sum-weighted n)
  (stream-take ordered-pairs-by-sum n))

(define (take-235-weighted n)
  (stream-take ordered-pairs-235 n))

(take-sum-weighted 10)
(take-235-weighted 10)
