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
                  ;; 同じ重みの要素を両方出力して進める
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

;; 三通り以上の二平方和で表される数を検出
(define (square x) (* x x))
(define (square-weight i j) (+ (square i) (square j)))

(define square-pairs
  (weighted-pairs integers integers square-weight))

(define (numbers-with-3-square-reps pair-stream)
  (let* ((p1 (stream-car pair-stream))
         (p2 (stream-car (stream-cdr pair-stream)))
         (p3 (stream-car (stream-cdr (stream-cdr pair-stream))))
         (w1 (square-weight (car p1) (cadr p1)))
         (w2 (square-weight (car p2) (cadr p2)))
         (w3 (square-weight (car p3) (cadr p3))))
    (if (and (= w1 w2) (= w2 w3))
        (cons-stream
         w1
         ;; 次へ進む（同じ重みがさらに続く場合もあるので1つだけ進める）
         (numbers-with-3-square-reps (stream-cdr pair-stream)))
        (numbers-with-3-square-reps (stream-cdr pair-stream)))))

(define three-square-sum-stream
  (numbers-with-3-square-reps square-pairs))

;; サンプル取得
(define (take-three-square-sums n)
  (stream-take three-square-sum-stream n))

(take-three-square-sums 10)
