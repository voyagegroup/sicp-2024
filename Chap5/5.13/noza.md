# 問題 5.13

`make-machine` からレジスタ名リストを削除し、引数を演算表と制御器だけにした。

```scheme
(make-machine ops controller-text)
```

`get-register` がアセンブル中に未知のレジスタ名を受け取った場合、`lookup-register` はその場で `allocate-register` を呼び、作成後のレジスタを返す。命令の実行手続きはアセンブル時に作られるため、制御器で初めて参照されたレジスタは実行前に自動で割り当てられる。
