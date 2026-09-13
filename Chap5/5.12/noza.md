# 問題 5.12

アセンブル時に `analyze-instructions` を実行し、結果を計算機へ保存するようにした。`machine-analysis` で次の4種類を取得できる。

- `instruction-types`: 異なる命令型
- `entry-point-registers`: `(goto (reg ...))` が参照するレジスタ
- `stack-registers`: `save` または `restore` の対象レジスタ
- `register-sources`: 各レジスタに代入される異なる値の式

たとえば、`x` への `(assign x <- (const 1))`、`(save x)`、`(restore x)`、`(goto (reg continue))` を含む制御器の解析結果は、命令型 `assign`・`save`・`restore`・`goto`、入口レジスタ `continue`、スタックレジスタ `x`、`x` の代入元 `(const 1)` を含む。

## 図 5.12 の Fibonacci 計算機

`noza.rkt` で `fib-machine` を定義し、`fib-machine-analysis` に解析結果を保存した。結果は次のとおり。

確認は次のコマンドで行える。

```sh
racket Chap5/5.12/noza.rkt
```

- 命令型: `assign`、`test`、`branch`、`goto`、`save`、`restore`
- 入口レジスタ: `continue`
- スタックレジスタ: `n`、`val`、`continue`
- `continue` の代入元: `(label fib-done)`、`(label afterfib-n-1)`、`(label afterfib-n-2)`
- `n` の代入元: `(op -) (reg n) (const 1)`、`(op -) (reg n) (const 2)`、`(reg val)`
- `val` の代入元: `(reg n)`、`(op +) (reg val) (reg n)`
