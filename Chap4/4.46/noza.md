# 問題 4.46

構文解析プログラムは `*unparsed*` というグローバル変数を `set!` で破壊的に更新しながら動作する。各 `parse-*` 関数は呼ばれた順に `*unparsed*` の先頭から語を消費していくため、評価順序が左から右であることが前提となっている。

例えば `parse-sentence` は：

```scheme
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)   ; ① 最初に呼ばれ冠詞・名詞を消費
        (parse-verb-phrase))) ; ② 残りの語を消費
```

左から右に評価されるので、① が先に `*unparsed*` から名詞句分の語を取り出し、② がその続きを動詞句として解析できる。

**右から左に評価されると**、② `parse-verb-phrase` が先に呼ばれ、`*unparsed*` の先頭にある `"the"` を動詞として解析しようとして `require` が失敗する。

同様に `parse-prepositional-phrase` でも：

```scheme
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions) ; ① 前置詞を消費
        (parse-noun-phrase)))     ; ② 残りを名詞句として解析
```

右から左だと ② が先に実行され、前置詞を名詞句として解析しようとして失敗する。

つまり、`*unparsed*` への破壊的代入が評価順序に依存しているため、被演算子が左から右に評価されなければプログラムは正しく動作しない。
