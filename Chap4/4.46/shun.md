例えば被演算子が右から左に評価するとすると、

```racket
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
```

これはparse-verb-phraseが先に評価される。
`*unparsed*`が(the cat eats)だとするとeatsが処理されて欲しいが、先頭がtheなのでparseをすることができない

```racket
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*))) ; found-word = the
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
```