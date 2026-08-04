#lang sicp
; 図4.5 https://sicp.iijlab.net/fulltext/x442.html
; 質問の直列組合せ(図4.5)としてのわれわれのand実装は美しいが, andの第二の質問の処理に, 第一の質問で作られた各フレームにつき, データベースを走査しなければならないので, これは非効率である. データベースにN個の要素があり, 代表的な質問はNに比例した数(例えばN/k)の出力フレームを作るとすれば, 第一の質問で作られたフレームにつき, データベースの操作はN2/k個のパターンマッチの呼出しを必要とするであろう

(and (job ?x ?job)
     (supervisor ?x ?boss))

; frame1: ((Hacker Alyssa P) (computer programmer))
; frame2: (job (Tweakit Lem E) (computer technician))
; frame3:  (job (Bitdiddle Ben) (computer wizard))
; それぞれのframeでさらに、全てのDBを捜査することになるので、重複したスキャンと判定をすることになっている。

; 別の解決法は, andの二つの節を別々に処理し, 出力フレームの矛盾しないすべての対を探すことである. 各質問がN/kの出力フレームを作り出すから, N2/k2回の無矛盾性のチェックを行う必要がある. --- 現在の方法に必要なマッチ数より, k倍少ない.

; frame1: ((Hacker Alyssa P) (computer programmer))
; frame2: (job (Tweakit Lem E) (computer technician))
; frame3:  (job (Bitdiddle Ben) (computer wizard))

; これにくわえて、以下をだす。

; frame1': ?x=Ben     ?boss=Warbucks
; frame2': ?x=Louis   ?boss=Ben
; frame3': ?x=Cy      ?boss=Ben

; それぞれが矛盾しないものを最終的なしゅつりょくとしてだす

; もとのconjoin
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))
(put 'and 'qeval conjoin)

; こんかいの
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge-frame-streams
       (qeval (first-conjunct conjuncts)
              frame-stream)
       (conjoin (rest-conjuncts conjuncts)
                frame-stream))))

(define (merge-frame-streams s1 s2)
  (stream-flatmap
   (lambda (frame1)
     (stream-flatmap
      (lambda (frame2)
        (let ((merged (merge-frames frame1 frame2)))
          (if (eq? merged 'failed)
              the-empty-stream
              (singleton-stream merged))))
      s2))
   s1))

(define (merge-frames frame1 frame2)
  (define (merge bindings result)
    (if (null? bindings)
        result
        (let* ((binding (car bindings))
               (var (binding-variable binding))
               (val (binding-value binding))
               (old (binding-in-frame var result)))
          (cond
            ((not old)
             (merge (cdr bindings)
                    (extend var val result)))

            ((equal? val (binding-value old))
             (merge (cdr bindings)
                    result))

            (else
             'failed)))))
  (merge frame2 frame1))