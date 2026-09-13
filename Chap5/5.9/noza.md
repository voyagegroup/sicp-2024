# 問題 5.9

演算式の被演算子には、`(const ...)` と `(reg ...)` だけを許可する。

`make-operation-exp` が各被演算子の実行手続きを作る前に、被演算子が定数式またはレジスタ式かを確認するようにした。

```scheme
(if (or (constant-exp? e)
        (register-exp? e))
    (make-primitive-exp e machine labels)
    (error "Operation operand must be a constant or register -- ASSEMBLE"
           e))
```

これにより、次のような式はアセンブリ時にエラーになる。

```scheme
(assign x (op +) (label done) (const 1))
```

一方で、通常のラベル代入は引き続き使える。

```scheme
(assign continue (label done))
```
