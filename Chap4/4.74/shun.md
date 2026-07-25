## a

```scheme
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter
               (lambda (s) (not (stream-null? s)))
               stream)))

(define (singleton-stream x) ;singleton-streamはemptyとのpair
  (cons-stream x the-empty-stream))
```

## b

```
(assert! (a 10))

(assert! (a 9))

(assert! (a 8))

(assert! (rule (same ?x ?x)))

(and (a ?x)
     (not (same ?x 10)))
```

```
;;; Query results:
(and (a 8) (not (same 8 10)))
(and (a 9) (not (same 9 10)))
```

結果は変わらない。
negate, lisp-value, find-assertionsはどれも各結果が空ストリームか単一ストリームしか返さない手続きである。
そのため、先頭要素だけを取るsimple版で同じことが実現できる。
