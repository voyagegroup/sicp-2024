# 問題 4.15 解答

`halts?` が次を満たす手続きだと仮定する。

- 任意の一引数手続き `p` とオブジェクト `a` について、`(p a)` が値を返して停止するなら真を返す
- 停止しないなら偽を返す

このとき次を定義する。

```scheme
(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
```

ここで `(try try)` を考える。

1. `(halts? try try)` が真の場合  
   `try` の定義より `(try try)` は `(run-forever)` を実行し、停止しない。  
   しかし `halts?` は「停止する」と判定していたので矛盾。

2. `(halts? try try)` が偽の場合  
   `try` の定義より `(try try)` は `'halted` を返して停止する。  
   しかし `halts?` は「停止しない」と判定していたので矛盾。

どちらの場合も矛盾するため、最初の仮定（任意の手続きと入力に対して正しく停止性を判定する `halts?` が存在する）は成り立たない。  
したがって、そのような万能な `halts?` を書くことは不可能である。
