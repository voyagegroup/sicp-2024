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

;; 問題 3.63
(define (average a b) (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

;; Alyssa の版: 自己参照のストリームを局所変数に束縛して共有する
(define sqrt-count/shared 0)
(define (sqrt-stream/shared x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (set! sqrt-count/shared (+ sqrt-count/shared 1)) ;計算回数をカウントする
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; Louis の版: (sqrt-stream x) を毎回呼び出すため、共有が効かずに再計算が爆発する
(define sqrt-count/recomputed 0)
(define (sqrt-stream/recomputed x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (set! sqrt-count/recomputed (+ sqrt-count/recomputed 1))
                             (sqrt-improve guess x)) ; 計算回数をカウントする
                           (sqrt-stream/recomputed x))))

;; 動作確認: それぞれ n 番目の推測値を 1 回だけ取り出す
(define sample-index 10)

(set! sqrt-count/shared 0)
(stream-ref (sqrt-stream/shared 2) sample-index)
sqrt-count/shared   ; => 10 （1 回ずつ進む）

(set! sqrt-count/recomputed 0)
(stream-ref (sqrt-stream/recomputed 2) sample-index)
sqrt-count/recomputed ; => 55 （同じ前半を何度も計算し直す）

; 解説:
;   shared 版では guesses という 1 つのストリームを作り、その cdr を遅延評価で 1 度だけ
;   拡張していくので、各項の計算は 1 回ずつで済む。
;   Louis の版では stream-cdr を強制するたびに (sqrt-stream x) という新しいストリーム
;   全体を構築し直すため、先頭から同じ部分を何度も再計算してしまい、指数的に遅くなる。
;   もし delay が memo-proc を使わず (lambda () ⟨exp⟩) だけなら遅延結果が共有されないため、
;   どちらの版も毎回再計算になり、効率の差はほぼなくなる。
