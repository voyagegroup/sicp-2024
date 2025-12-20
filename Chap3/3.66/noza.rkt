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

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-take s k)
  (if (or (zero? k) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s) (- k 1)))))

;; interleave: 無限ストリームでも交互に混ぜるので後者も必ず現れる
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; 整数ストリーム
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;; pairs: SICP 本文通り interleave で行頭と残りを混ぜる
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; (pairs integers integers) の先頭を覗いて順序を確認
(define first-20-pairs (stream-take (pairs integers integers) 20))

;; 並び方のざっくり説明:
;;   (1) 先頭 (1,1) を出す
;;   (2) 行頭 (1,2),(1,3)... と 1 つ右下のサブ問題 (2,2),(3,3)... を interleave で交互に混ぜる
;;   (3) その再帰で各対角線の先頭を優先しつつジグザグに全対角線を走査する
;; （漸化式は AI に解いてもらいました・・・）
;; 位置を解析する closed form （前の回答を保持）
(define (pair-index i j)
  (cond ((> i j) (error "pair-index: require i <= j"))
        ((and (= i 1) (= j 1)) 0)
        ((= i 1) (- (* 2 j) 3))          ; 2,3,4,... の行頭は 1,4,5,...
        (else
         (let* ((k (- i 1))
                (m (+ 1 (- j i)))       ; m = j - i + 1
                (base (if (= m 1) 0 (- (* 2 m) 3)))
                (pow2 (expt 2 k)))
           (+ (* pow2 base)
              (* 2 (- pow2 1)))))))

;; 問題文で尋ねられた例
(define idx-1-100   (pair-index 1 100))          ; 197
(define idx-99-100  (pair-index 99 100))         ; 3 * 2^98 - 2
(define idx-100-100 (pair-index 100 100))        ; 2^100 - 2

;; 動作確認のため値を出力
first-20-pairs
idx-1-100
idx-99-100
idx-100-100
