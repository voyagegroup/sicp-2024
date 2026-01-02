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

(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; 整数ストリーム
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;; 重み付きマージ (weight : i j -> 数値)
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
                  ;; 同じ重みの要素を両方出力
                  (cons-stream x1
                               (cons-stream x2
                                            (merge-weighted (stream-cdr s1)
                                                            (stream-cdr s2)
                                                            weight)))))))))

;; i <= j のペアを重み順に生成 (weight : i j -> 数値)
(define (weighted-pairs s t weight)
  (let ((i0 (stream-car s))
        (t-rest (stream-cdr t)))
    (cons-stream
     (list i0 (stream-car t))
     (merge-weighted
      (stream-map (lambda (x) (list i0 x)) t-rest) ; 同じ i0 の行
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight) ; 右下の行列
      weight))))

;; Ramanujan 数 (2通り以上の立方和)
(define (cube x) (* x x x))
(define (cube-weight i j) (+ (cube i) (cube j)))

(define cube-pairs
  (weighted-pairs integers integers cube-weight))

(define (ramanujan-numbers pair-stream)
  (let* ((p1 (stream-car pair-stream))
         (p2 (stream-car (stream-cdr pair-stream)))
         (w1 (cube-weight (car p1) (cadr p1)))
         (w2 (cube-weight (car p2) (cadr p2))))
    (if (= w1 w2)
        ;; 同じ重みが続くとき、その重みを Ramanujan 数として返す
        (cons-stream w1
                     (ramanujan-numbers (stream-cdr pair-stream)))
        (ramanujan-numbers (stream-cdr pair-stream)))))

(define ramanujan-stream (ramanujan-numbers cube-pairs))

;; サンプル取得
(define (take-ramanujan n)
  (stream-take ramanujan-stream n))

(take-ramanujan 10)
