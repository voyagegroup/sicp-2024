# 問題 5.10

`assign` 命令に、代入先と値の区切りを明示する新しい構文を導入する。

```scheme
(assign <レジスタ名> <- <値の式>)
```

使用例は次のとおり。

```scheme
(assign a <- (const 3))
(assign a <- (op +) (reg a) (const 1))
```

`make-assign` は `assign-reg-name` と `assign-value-exp` を通じて命令の部品を取得している。そのため、次の二つの構文手続きだけを変更すれば、命令の実行機構を変えずに新しい構文を使える。

```scheme
(define (assign-reg-name assign-instruction)
  (if (eq? (caddr assign-instruction) '<-)
      (cadr assign-instruction)
      (error "Bad ASSIGN instruction -- ASSEMBLE"
             assign-instruction)))

(define (assign-value-exp assign-instruction)
  (if (eq? (caddr assign-instruction) '<-)
      (cdddr assign-instruction)
      (error "Bad ASSIGN instruction -- ASSEMBLE"
             assign-instruction)))
```

`<-` がない旧構文はアセンブリ時にエラーになる。
