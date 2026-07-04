letを使わずに以下のように定義したとする。

```
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)
```

この時、THE-ASSERTIONSがcdrにTHE-ASSERTIONS自身を持つストリームとなり、無限ストリームになってしまう。

```
(set! THE-ASSERTIONS
        (cons-stream A THE-ASSERTIONS))
```

としたcdrもAになってしまう。

letで一度束縛することで

```
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))
```

```
(set! THE-ASSERTIONS
          (cons-stream A old-assertions))
```

とした場合もstream THE-ASSERTIONSは`A empty`と想定した形になる。