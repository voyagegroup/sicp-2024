# 問題 5.11

`save` と `restore` の意味には、互いに異なる三つの設計がある。a、b、c は同時に採用するものではなく、別々のシミュレータ実装として考える。

## a. 現在の単一スタック

現在のシミュレータでは、スタックには値だけを積む。したがって、`restore` の対象レジスタ名と、値を保存したレジスタ名は対応付けられない。


## b. 保存元と復元先を一致させる単一スタック

スタックに値だけでなく保存元のレジスタ名も積む。`restore y` はスタック先頭の保存元が `y` の場合だけ復元し、それ以外はエラーにする。

```scheme
(save y)    ; スタックへ (y . <y の値>) を積む
(save x)    ; スタックへ (x . <x の値>) を積む
(restore y) ; 先頭は x なのでエラー
```
`noza-b.rkt` の `make-machine` は、この方式で動く。

## c. レジスタごとのスタック

各レジスタに個別のスタックを持たせる。`save y` は `y` 専用スタックへ積み、`restore y` はそこから取り出す。他レジスタの保存・未復元の値は影響しない。

この方式では、レジスタの割当て時に対応するスタックも作り、`initialize-stack` は全レジスタ用スタックを初期化する必要がある。

`noza-c.rkt` の `make-machine` はこの方式で計算機を作る。たとえば `(save x)`、`(save y)` の後でも `(restore x)` は `x` 専用スタックから値を復元できる。

実装では、計算機モデルに `register-stacks` という表を追加する。これは各レジスタ名と、そのレジスタ専用スタックを対応付ける。

```scheme
((x <x 用スタック>)
 (y <y 用スタック>)
 ...)
```

レジスタを割り当てる `allocate-register` は、レジスタオブジェクトと同時に `make-stack` で専用スタックを作って `register-stacks` に登録する。さらに、`get-register-stack` メッセージで名前から専用スタックを取得できるようにする。

`save` と `restore` は、共通の `stack` ではなく、その命令が指定するレジスタ名から取得した専用スタックを使う。

```scheme
; (save x)
(let ((reg-stack ((machine 'get-register-stack) reg-name)))
  (push reg-stack (get-contents reg)))

; (restore x)
(let ((reg-stack ((machine 'get-register-stack) reg-name)))
  (set-contents! reg (pop reg-stack)))
```

最後に、`initialize-stack` は共通スタックだけでなく、`register-stacks` に入っている全スタックも初期化する。
