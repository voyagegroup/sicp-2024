# 問題 4.20 解答

## a. `letrec` を導出された式として実装

`Chap4/4.20/noza.rkt` で以下を実装した。

- `letrec?`
- `letrec-bindings`, `letrec-body`
- `letrec-variables`, `letrec-values`
- `letrec->let`
- `eval` への分岐追加  
  `((letrec? exp) (eval (letrec->let exp) env))`

変換規則:

```scheme
(letrec ((v1 e1) ... (vn en))
  body...)
```

を

```scheme
(let ((v1 '*unassigned*) ... (vn '*unassigned*))
  (set! v1 e1) ... (set! vn en)
  body...)
```

へ変換して評価する。

---

## b. `letrec` と `let` の環境ダイアグラム比較

`f` は問題文のもの（`even?` と `odd?` の相互再帰）を使う。  
`(f 5)` の評価中、`⟨fの本体の残り⟩`（ここでは `(even? x)`）を評価する時点を比較する。

### 1) `letrec` 版

```scheme
(define (f x)
  (letrec ((even? (lambda (n) ... odd? ...))
           (odd?  (lambda (n) ... even? ...)))
    (even? x)))
```

環境（概略）:

```text
GE: f -> <procedure env=GE>

E1 (f呼び出し): x=5, parent=GE

E2 (letrec本体): even?=<procedure env=E2>
                 odd? =<procedure env=E2>
                 parent=E1
```

`E2` の中で `(even? x)` を評価する。  
`even?` と `odd?` のクロージャはどちらも `E2` を環境として捕捉しているため、相互再帰呼び出し時に互いを参照できる。

### 2) `let` 版（`letrec` を単純に `let` へ置換）

```scheme
(define (f x)
  (let ((even? (lambda (n) ... odd? ...))
        (odd?  (lambda (n) ... even? ...)))
    (even? x)))
```

`let` では初期化式（右辺）を**外側環境 E1**で先に評価してから、新しいフレームを作る。  
その結果:

```text
GE: f -> <procedure env=GE>

E1 (f呼び出し): x=5, parent=GE

even? の右辺 lambda は E1 を捕捉
odd?  の右辺 lambda も E1 を捕捉

E2 (let本体): even?=<procedure env=E1>
              odd? =<procedure env=E1>
              parent=E1
```

`E2` で `(even? x)` を呼ぶと、`even?` 本体内の `odd?` 参照は捕捉環境 `E1` で探される。  
しかし `E1` には `odd?` 束縛がないので未束縛エラーになる。

---

## Louis の見落とし

「`define` の代わりに `let` でよい」という主張の抜けは、**クロージャがどの環境を捕捉するか**である。  
相互再帰には「互いの名前を含む同一環境」を手続きが捕捉する必要があり、それを満たすのが `letrec`（または 4.16 の `*unassigned*` + `set!` 変換）である。  
単純な `let` はこの条件を満たさない。
