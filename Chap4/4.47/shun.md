元の手続き

```racket
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
```


以下の手続きであれば今までと変わらずに動作する。

```racket
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))
```

amb式を入れ替えた場合は動作しない。

```racket
(define (parse-verb-phrase)
  (amb (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))
             (parse-word verbs))
        (parse-word verbs))
```

これはparseを行う前に`(parse-verb-phrase)`を内部で呼び出してしまい、無限ループになる。