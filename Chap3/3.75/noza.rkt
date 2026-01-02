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

;; Louis の構造を保ちつつ、平滑値の前回値を持ち回る形に修正
;; last-raw: 前回の生データ
;; last-smooth: 前回の平滑値
;; 前回のコードでは last-value の生のデータを扱っているのが間違い
(define (make-zero-crossings input-stream last-raw last-smooth)
  (let* ((current-raw (stream-car input-stream))
         (current-smooth (/ (+ current-raw last-raw) 2)))
    (cons-stream
     (sign-change-detector current-smooth last-smooth) ; 平滑値同士で符号判定
     (make-zero-crossings (stream-cdr input-stream)
                          current-raw         ; 次のステップの生データとして保持
                          current-smooth))))  ; 次のステップの平滑値として保持

;; エントリポイント: sense-data を受け取り初期値 0 で開始
(define (zero-crossings sense-data)
  (make-zero-crossings sense-data 0 0))

;; サンプル信号（3.74 と同様）
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
;; 例: (stream-take sample-zero-crossings 13)
