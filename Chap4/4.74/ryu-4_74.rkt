#lang sicp

; a. Alyssaのプログラムの欠けた式を補え.
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map ⟨??⟩
              (stream-filter ⟨??⟩ stream)))

; nagate
(not (job ?x accounting))
; lisp-value
(lisp-value > ?salary 30000)
; 

; stream-flatmap
; 2.2.3節で通常のリストに対して説明したflatmap手続きを, ストリームに変えたものである. しかし通常のflatmapとは違い, 単に連接するのではなく, 差し込んだプロセスによりストリームをを使って蓄積する(問題4.72および4.73参照).
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))


(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

; 例えば、nagate

define (negate operands frame-stream)
(stream-flatmap
 (lambda (frame)
   (if (stream-null? (qeval (negated-query operands)
                            (singleton-stream frame)))
       (singleton-stream frame)
       the-empty-stream))
 frame-stream))

#| 挙動をデバッグしてみる
;;; Query results:
nagate-input-frame:(((? job) . accounting) ((? x) Reasoner Louis))

nagate-input-frame:(((? job) . computer) ((? x) Hacker Alyssa))

(and (job (Hacker Alyssa) computer) (not (job (Hacker Alyssa) accounting)))
nagate-input-frame:(((? job) . computer) ((? x) Bitdiddle Ben))

(and (job (Bitdiddle Ben) computer) (not (job (Bitdiddle Ben) accounting)))


;;; Query input:
(and
  (job ?x ?job)
  (not (job ?x accounting)))

;;; Query results:
nagate-input-frame:(((? job) . accounting) ((? x) Reasoner Louis)) ; 1

nagate-input-frame:(((? job) . computer) ((? x) Hacker Alyssa)); 2

(and (job (Hacker Alyssa) computer) (not (job (Hacker Alyssa) accounting)))
nagate-input-frame:(((? job) . computer) ((? x) Bitdiddle Ben)) ; 3

(and (job (Bitdiddle Ben) computer) (not (job (Bitdiddle Ben) accounting)))


1は、?jobがaccounting, ?xがReasonerに束縛されている。
accountingじゃないので、filterされて、the-empty-streamがかえる。

2は、?jobがcomputer、?xがHackerに束縛されている。
computerなので、(singleton-stream frame)がかえる。

3も2とどうよう。

→ streamは、empty or (singleton-stream stream)がかえる

simple-flattenは、streamから、emptyをとりのぞくか、先頭のsingleton-streamをとりだす
|#

(define (simple-flatten stream)
  (stream-map
   stream-car ; ?? singleton-streamを取り出す
   (stream-filter
    (lambda (s) (not (stream-null? s))) ; ?? sが空かどうかの判定
    stream)))

; b. このように変更すると, 質問システムの振舞いは変るか.
; empty or 1つのstreamにおいて、simpleを使うのであれば、混ぜ合わせる処理の必要性はないので、振る舞いはかわらないと考える。
