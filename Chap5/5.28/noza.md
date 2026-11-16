# 問題 5.28

`Chap5` で `racket 5.28/noza.rkt` を実行する。5.26 の監視つき評価器を元に、`ev-sequence` を5.4.2節の末尾再帰でない版に変更した。入力する階乗の定義は5.26と5.27の `noza.rkt`・`noza.md` と同じ。

```scheme
ev-sequence
  (test (op no-operands?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))

ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))

ev-sequence-end
  (restore continue)
  (goto (reg continue))
```

空の並びの判定には、既存の `no-operands?`（`null?`）を利用した。最後の式も `unev` と `env` を保存して評価し、並びが空になってから `continue` を復元する。

定義時の統計を除いた実測値は次のとおり。

| n | 反復版のプッシュ回数 | 反復版の最大深さ | 再帰版のプッシュ回数 | 再帰版の最大深さ |
|---:|---:|---:|---:|---:|
| 1 | 70 | 17 | 18 | 11 |
| 2 | 107 | 20 | 52 | 19 |
| 3 | 144 | 23 | 86 | 27 |
| 4 | 181 | 26 | 120 | 35 |
| 5 | 218 | 29 | 154 | 43 |
| 10 | 403 | 44 | 324 | 83 |

`n ≥ 1` に対する式でまとめると、次のようになる。

| 評価器 | 階乗 | プッシュ回数 | 最大深さ |
|---|---|---:|---:|
| 元の評価器 | 反復版 | 35n + 29 | 10 |
| 今回の評価器 | 反復版 | 37n + 33 | 3n + 14 |
| 元の評価器 | 再帰版 | 32n − 16 | 5n + 3 |
| 今回の評価器 | 再帰版 | 34n − 16 | 8n + 3 |

反復版では `factorial` の最後の式と `iter` の本体を評価するたびに2回余計にプッシュする。`factorial` は1回、`iter` は終了判定を含め `n + 1` 回呼ばれるため、追加分は `2(n + 2)` 回となる。最大深さは `n = 1` で17、反復が1回増えると、戻り先・`unev`・`env` の3個がさらに残り、`17 + 3(n − 1) = 3n + 14` となる。

再帰版では `factorial` が `n` 回呼ばれ、それぞれの本体で2回余計にプッシュするため、追加分は `2n` 回。最大深さは `n = 1` で11、再帰が1段増えるたび、元の5個に並びの評価用の3個が加わるので、`11 + 8(n − 1) = 8n + 3` となる。

どちらの階乗も最大深さが入力に比例する。反復的な手続きの書き方だけでは十分ではなく、評価器が末尾位置の呼び出しで退避を残さないことが、一定スペースでの実行に必要になる。
