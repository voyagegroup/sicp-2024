#lang sicp
; --- 3.5.1

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; (stream-car (cons-stream x y)) = x
(define (stream-car stream) (car stream))
; (stream-cdr (cons-stream x y)) = y
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; --- 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; --- 3.52

; sumの初期化
(define sum 0)

; sumにxを足す
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; 偶数だけ
(define y (stream-filter even? seq))
; 5の倍数
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
seq
; (1 . #<promise>)
y
; (6 . #<promise>)
z
; (10 . #<promise>)

(stream-ref y 0)

(stream-ref y 7)

; 136
; sec0:
;   sum = (+ 1 0) = 1
;   (even 1) = #f
; sec1:
;   sum = (+ 2 1) = 3
;   (even 3) = #f
; sec2:
;   sum = (+ 3 3) = 6
;   (even 6) = #t
;   y0 = 6
; sec3:
;   sum = (+ 4 6) = 10
;   (even 10) = #t
;   y1 = 10
; sec4:
;   sum = (+ 5 10) = 15
;   (even 15) = #f
; sec5:
;   sum = (+ 6 15) = 21
;   (even 21) = #f
; sec6
;   sum = (+ 7 21) = 28
;   (even 28) = #t
;   y2=28
; ...
; sec14
;   sum = (+ 15 105) = 120
;   y6 = 120
; sec15
;   sum = (+ 16 120) = 136)
;   y7 = 136
(display 'z)
(newline)


(display-stream z)
; sumが5の倍数の時、displayをする
#|
10
15
45
55
105
120
190
210
|#

; この応答は(delay ⟨exp⟩)を単に(lambda () ⟨exp⟩)で実装し, memo-procの用意する最適化を使わなかった時とどう違うか. 説明せよ.
; memo-procは、「メモ化手続きが最初に走った時, 計算結果を退避しておく. その後の評価では, 単にその結果を返す.」仕様である。
; memo-procは結果を対比しておくため、sumへの副作用が、各seqにおいて1回ずつになる。
; もし、memo-procを使わないと、それぞれの手続きでsumに副作用が生じてしまう。

; 例えば、以下のような手続きをする
(stream-ref z 0)
(stream-ref z 0)
(stream-ref z 1)

; メモ化がある場合:
(stream-ref z 0) ; 10, 初回アクセスなのでsecc3まで評価され、メモ化される
(stream-ref z 0) ; 10, メモ化ずみなので、accumはよばれない
(stream-ref z 0) ; 15, 未評価の部分だけ、accumがよばれる

; メモ化がない場合
(stream-ref z 0) ; 10
(stream-ref z 0) ; 20, accumが再度よばれ、sumを更新する
(stream-ref z 0) ; 35, accumが再度よばれ、sumを更新する

