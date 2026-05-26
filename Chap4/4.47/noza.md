# 問題 4.47

## Louis の定義

```scheme
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))
```

## これは動くか？

**理論的には等価だが、実装上は問題がある。** amb-all で全解を列挙する際に**無限ループに陥る**。

### Louis 版

```scheme
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)        ; ← 「計算」を毎回 call
             (parse-prepositional-phrase))))
```

### maybe-extend 版

```scheme
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase                    ; ← 「値」が bind されている
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
```

---

## 無限ループの仕組み

### Louis 版（無限ループ）

amb-all で全解を列挙する流れ：

```
選択肢1: (parse-word verbs)
  → *unparsed* = '() で成功 → fail を呼ぶ

選択肢2 を try: (list 'verb-phrase (parse-verb-phrase) ...)
  ↓
  (parse-verb-phrase) を evaluate
    ↓
    amb の選択肢1: (parse-word verbs)
      → *unparsed* = '() で require fail ✗
    ↓
    amb の選択肢2: (list 'verb-phrase (parse-verb-phrase) ...)
      → (parse-verb-phrase) を call  ← また新しい amb が created
        ↓
        またも選択肢1: fail
        またも選択肢2: (parse-verb-phrase) を call
          ↓
          無限に nested amb が増える...
```

**問題：** 「計算 `(parse-verb-phrase)` を毎回 call する」ため、amb-all が「その計算の all solutions」を探そうとして、nested が無限に深くなる。

### maybe-extend 版（有限で止まる）

```
選択肢1: verb-phrase を返す
  → 成功 → fail を呼ぶ

選択肢2 を try: (maybe-extend (list 'verb-phrase verb-phrase ...))
  ↓
  新しい verb-phrase に対して maybe-extend を call
  ↓
  その中の amb で選択肢1 / 選択肢2 を試す
    選択肢1: return
    選択肢2: (parse-prepositional-phrase) を try
      → *unparsed* = '() で require fail ✗
  ↓
  maybe-extend の選択肢2 も fail
  ↓
  有限個の depths で stop ✓
```

**理由：** `verb-phrase` という**「値」が parameter に bind されている**ため、amb-all は「その値に対する拡張の有無」という有限個の choices しか試さない。
