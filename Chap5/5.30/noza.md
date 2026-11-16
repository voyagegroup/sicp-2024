# 問題 5.30

`Chap5` で `racket 5.30/noza.rkt` を実行する。

5.26 の監視つき評価器と `0.0/noza-evaluator-support.rkt` を元に、エラー処理用の変更をこの問題の `noza.rkt` と `noza-support.rkt` にまとめた。元の共通実装は変更していない。

## a. 評価プロセスのエラー

条件コードは、非公開のタグを先頭に持つベクタで表す。

```scheme
(define error-tag (list 'evaluation-error))

(define (make-evaluation-error kind details)
  (vector error-tag kind details))

(define (evaluation-error? value)
  (and (vector? value)
       (= (vector-length value) 3)
       (eq? (vector-ref value 0) error-tag)))
```

単なる記号を条件コードにすると、その記号を引用した正常な値と衝突する。非公開タグを `eq?` で比較することで、利用者が同じ内容のデータを引用しても条件コードとして扱われない。

既存の環境操作のうち、基盤 Scheme の `error` を呼んでいたすべての箇所を変更した。

| 環境操作 | 失敗条件 | 評価器での検査箇所 |
|---|---|---|
| `lookup-variable-value` | 未束縛変数 | `ev-variable` |
| `set-variable-value!` | 未束縛変数への代入 | `ev-assignment-1` |
| `extend-environment` | 合成手続きの引数過多・不足 | `compound-apply` |

変数の参照では次のように検査する。

```scheme
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (test (op evaluation-error?) (reg val))
  (branch (label signal-error))
  (goto (reg continue))
```

代入は `perform` から `assign val` に変更し、結果が条件コードかを検査した後、成功時にだけ `ok` を返す。環境拡張は結果が `env` に入るため、失敗時に `val` へ移して `signal-error` へ進む。

`define-variable!` は通常の環境では新規束縛の追加か既存束縛の更新に成功するため、変更は不要。定義・代入の右辺の評価が失敗した場合も `ev-variable` などから直接脱出するので、未評価の値を束縛することはない。

## b. 基本手続きのエラー

登録済みの基本手続き11個をすべて `checked-primitive` で包み、実装の呼び出し前に検査する。

| 基本手続き | 個数・型の検査 |
|---|---|
| `car`, `cdr` | 1引数、対であること |
| `cons` | 2引数 |
| `null?` | 1引数 |
| `+`, `*` | 0個以上、すべて数値 |
| `-`, `/` | 1個以上、すべて数値 |
| `=` | 2個以上、すべて数値 |
| `<`, `>` | 2個以上、すべて実数 |

`/` はさらに除数が0でないことを調べる。1引数の `(/ x)` では `x` 自身、複数引数では第2引数以降を検査する。したがって `(/ 0 2)` は正常な計算として0を返す。

基本手続きが返した条件コードは `primitive-apply` で検査する。

```scheme
primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (test (op evaluation-error?) (reg val))
  (branch (label signal-error))
  (restore continue)
  (goto (reg continue))
```

`signal-error` は条件コードを読みやすいリストとして表示し、入力ループへ戻る。残ったスタックは入力ループの `initialize-stack` で破棄するため、エラー経路で途中の継続を復元する必要はない。正常な基本手続き呼び出しでは従来どおり `continue` を復元する。

## 実行例

```scheme
;;; EC-Eval input:
(+ 1 missing)
(error unbound-variable missing)

;;; EC-Eval input:
(set! missing 3)
(error unbound-variable-set! missing)

;;; EC-Eval input:
((lambda (x) x))
(error too-few-arguments ())

;;; EC-Eval input:
(/ 1 0)
(error division-by-zero /)

;;; EC-Eval input:
(car 'symbol)
(error wrong-argument-type car)

;;; EC-Eval input:
(car)
(error wrong-argument-count car)

;;; EC-Eval input:
(+ 20 22)
(total-pushes = 8 maximum-depth = 5)
;;; EC-Eval value:
42
```

`python3 5.30/noza-test.py` で、環境操作と全基本手続きの失敗、各失敗後の入力ループへの復帰、正常な計算を確認できる。

この実装の対象は、正しい構文の式における上記の実行時エラー。壊れた構文の検査や読込みエラー、資源枯渇などを含む完全なエラーシステムは対象に含めない。
