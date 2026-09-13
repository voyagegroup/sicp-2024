# 問題 5.8

修正前のシミュレータでは、制御が `there` に達した時点でレジスタ `a` の内容は `4` になる。

`extract-labels` は制御器テキストを末尾から先頭へ再帰的に処理し、見つけたラベルエントリを `labels` の先頭へ追加する。そのため、二つある `here` のうち、後ろに定義された `here` がラベル表では先に現れる。

`lookup-label` は `assoc` で最初に見つかったエントリを使うので、最初の命令

```scheme
(goto (label here))
```

は二つ目の `here`、すなわち次の命令へジャンプする。

```scheme
(assign a (const 4))
```

よって `a` に `4` が代入された後、`there` へ進む。

重複ラベルをエラーにする修正は、ラベルを追加する前に `(assoc next-inst labels)` で同名のラベルが登録済みか確認し、見つかった場合は `Duplicate label -- ASSEMBLE` エラーにする。
