#lang sicp

; simple-query
; -- 本文の実装 --

; simple-query手続きは単純な質問を扱う. 引数として単純質問(パターン)とフレームのストリームをとり, 各フレームを質問の全データベースのマッチで拡張して形成されたストリームを返す.
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))


; --- Reasuner版 ---

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame))) ; delayがない
   frame-stream))

; --- simple-query内でつかう手続き

; find-assertionsは入力としてパターンとフレームをとる. 与えられたフレームをパターンとのデータベースマッチで拡張したフレームのストリームを返す. 
(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

; apply-rulesはfind-assertions(4.4.4.3節)に似た規則である. これは入力としてパターンとフレームをとり, データベースの規則を作用させて, 拡張されたフレームのストリームを形成する.
; stream-flatmapは(fetch-rules で選択した, 4.4.4.5節)作用させられそうな規則のストリームにapply-a-ruleを順にマップし, 結果のフレームのストリームを組み合せる.
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))


; 動作を見てみる
(assert! (married Minnie Mickey))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))
(married Mickey ?who)
; 本文バージョンの出力
(married Mickey Minnie)
(married Mickey Minnie)
(married Mickey Minnie)
.... ; ループ

; Reasunerバージョンの出力
; なにも出ないでループ

; なにが起きているか
; Schemeは作用的順序であり、delayを使うことで正規順序のしている。
; 本文バージョンは、ループをしながら、(find-assertions query-pattern frame)で見つかった結果を無限に出力をしていた。
; 一方、Reasuerバージョンは、作用的順序であり、(find-assertions query-pattern frame) で結果を見つけたとしても、(apply-rules query-pattern frame)の手続きも評価され、この評価が完了するまでstream-appendが実行されない。
; しかし、apply-rules → 本体をqeval → simple-query → apply-rule → ...とループするので、なにも出力されずにループしていた。



; disjoin
; -- 本文の実装 --
; or質問も図4.6に示すように, 同様に扱う. orのそれぞれの選言肢[disjunct]に対する出力ストリームは別々に計算され, 4.4.4.6節の interleave-delayed手続きを使って混ぜ合せる.
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

; --- Reasuner版 ---

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame))) ; delayがない
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream)))) ; こっちもdelayがない

; これを実行する
(or (job ?x (computer programmer))
    (married Mickey ?who))

; simple-queryと同様の事が起こる。
; 本文は正規順序なので、1つめの質問のjob側は出力された上で、ループする。
; Reasunerバージョンは、1つめ質問で答えが見つかったとしても、2つ目の質問のmarriedがループするのでなにも出力されない。
