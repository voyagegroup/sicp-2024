## 本文

```scheme
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
```

## Alyssa

```scheme
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))
```

## 並びが一つの式しか持たない場合

以下の式を考える。

```scheme
(begin proc1)
```

### 本文

`(null? rest-procs)`がtrueで、first-procをそのまま返すので単純に以下の式が返る。

```scheme
proc1
```

### Alyssa

lambdaで包んだ後以下のような式が返る。

```scheme
(lambda (env) (execute-sequence procs env))
```

実行時にexecute-sequenceが評価され、`(null? (cdr procs))`がtrueとなり手続きが取り出される。
つまり、実行するためのステップが多い。
また、実行するまで並びがいつ終端を迎えるかの解析が行われていない。

## 二つの式を持つ並びの場合

```scheme
(begin proc1 proc2)
```

### 本文

sequentiallyが呼ばれて、以下のような形の式が返る。

```scheme
(lambda (env) (proc1 env) (proc2 env))
```

３つ以上であれば前の要素ペアの後ろに新しい要素が追加されていく。
結果、並びの終端など気にせずにprocをただ順に評価していくだけで良い状態になっている。

### Alyssa

一要素の時と変わらず、lambdaで包んだ以下の式が返る。

```scheme
(lambda (env) (execute-sequence procs env))
```

実行の段になって、終端のチェックなどを行いながら評価されていく。

## まとめ

Alyssaの版は直接lambdaを返してしまうので並びの解析を行うことができない。
関数の呼び出しが一度だけであれば問題はないが、何度も呼ばれてしまうと毎度並びの解析が走ってしまうため効率が悪い。
本文はloop関数を定義して先に並びも解析している。
そのため、並びの解析は一度で済む。