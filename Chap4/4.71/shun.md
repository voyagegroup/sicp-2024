```
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream))))

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))
```

delayを置かない場合、schemeは最小の部分式について評価を行う。
よって元の定義がdelayしていた部分が最初に評価されてしまう。
ここではapply-rulesとdisjoinが遅延されずに実行される。
それぞれ中には再帰処理があり、無限ループの可能性を孕む。

simple-queryについてみると、以下のような時に無限ループする。

```
(p a)

(rule (p ?x)
        (p ?x))
```

ruleの評価が回り続けてしまうからだ。

disjoinについてみると、以下のようば時に無限ループする。

```
(base a)

(rule (p ?x)
        (or (base ?x)
            (p ?x)))
```

disjoinの中で同じ(p ?x)が呼ばれ続けてしまうためだ。

delayがあれば、この評価を遅延によって行わないため、無限ループしない。
