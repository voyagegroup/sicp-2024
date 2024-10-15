#lang sicp
;fast-exptのように, 逐次平方を使い, 対数的ステップ数の反復的べき乗プロセスを生成する手続きを設計せよ.
; (ヒント: (bn/2)2 = (b2)n/2に注意し, 指数nと底bの他にもう一つの状態変数aを用意する.
; 状態の移変りで積abnが不変であるように状態遷移を定義する.
; プロセス開始時にaを1とし, プロセス終了時にaが結果になるようにする.
; 一般に, 状態の移変りに不変のままの 不変量 (invariant quantity)を定義する技法は, 反復的アルゴリズムの設計に有用である.)

(define (square n) (* n n))
(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* b a)))))

(define (expt b n)
  (expt-iter b n 1))

(expt 2 4)
(expt-iter 2 4 1)
(expt-iter 4 2 1)
(expt-iter 16 1 1)
(expt-iter 16 0 16)
; -> 16

(expt 2 5)
(expt-iter 2 5 1)
(expt-iter 2 4 2)
(expt-iter 4 2 2)
(expt-iter 16 1 2)
(expt-iter 16 0 32)
; -> 32

(expt 2 6)
(expt-iter 2 6 1)
(expt-iter 4 3 1)
(expt-iter 4 2 4)
(expt-iter 16 1 4)
(expt-iter 16 0 64)
; -> 64

(expt 3 3)
(expt-iter 3 3 1)
(expt-iter 3 2 3)
(expt-iter 9 1 3)
(expt-iter 9 0 27)
; -> 27

;;;; 試行錯誤のログ

;(define (expt-iter b n a)
; (cond ((= n 0) a)
;       ((= n 1) a)
;       ((even? n) (expt-iter (square b) (/ n 2) (square a)))
;       (else (* b (expt-iter (square b) (- n 1) a)))))
; これでは、再帰的だ


;(define (expt-iter b n a)
;  (cond ((= n 0) a)
;        ((= n 1) a)
;        ((even? n) (if (= a 1)
;                       (expt-iter b (/ n 2) (square b))
;                       (expt-iter b (/ n 2) (square a))))
;        (else (* a (expt-iter b (- n 1) a)))))
; -> (expt 2 5) が16とおかしくなる。原因は、5は最初にelseにいくから、1が帰って、おかしくなる。 


; (define (expt-iter b n a)
;  (cond ((= n 0) a)
;        ((= n 1) a)
;         ((even? n) (expt-iter (square b) (/ n 2) (square b)))
;         (else (* b (expt-iter b (- n 1) a)))))
; 一応、それっぽくうごくようになったが、squareを2回計算しているから、計算量どうなんだろう。あと、(= n 1)の分岐消せないかな？
