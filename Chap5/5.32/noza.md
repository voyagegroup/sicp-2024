# 問題 5.32

## a. 演算子が変数である組合せの最適化

### 式の判別

演算子が変数である組合せを、次の述語で判別する。

```scheme
(define (variable-operator-application? exp)
  (and (application? exp)
       (variable? (operator exp))))
```

`(f 'x 'y)` の演算子 `f` は変数である。その評価は、現在の環境から束縛された値を探すだけであり、手続きの呼出しはまだ行わない。この探索では `env` は変更されない。

一方、`((f) 'x 'y)` の演算子 `(f)` は手続き呼出しであり、この特別扱いの対象にはならない。

述語を機械演算に登録し、`eval-dispatch` の一般的な `application?` の判定の直前に、次を追加する。自己評価式や特殊形式の判定順序は維持する。

```scheme
(test (op variable-operator-application?) (reg exp))
(branch (label ev-variable-application))
```

### 演算子の直接探索

新しい入り口では、被演算子を先に取り出してから、演算子の値を直接 `proc` に入れる。

```scheme
ev-variable-application
  (save continue)
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (assign proc (op lookup-variable-value) (reg exp) (reg env))
  (goto (label ev-appl-start-operands))
```

汎用の `eval-dispatch` を通じて演算子を評価する必要がなくなり、`env` と `unev` は変更されない。このため、元の `ev-application` にあった両レジスタの退避・回復を省ける。呼出し元へ戻るための `continue` の保存は残す。

## b. 特別扱いを増やせば翻訳系の利点をすべて除けるか

特別な場合を認識させることで、一部の最適化は評価器にも取り入れられる。しかし、それだけで翻訳系の利点をすべてなくせるわけではない。

今回の最適化でも、評価器は手続き作用を評価するたびに、その演算子が変数かどうかを判定する。繰り返し呼び出される手続きの本体では、同じ構文について判定が繰り返される。特別扱いを増やすほど、そのための実行時の判定も必要になる。

翻訳系は、構文の判別やレジスタの使用状況の解析を翻訳時に行う。実行時には、その結果を反映した命令列を直接実行できるため、同じ構文の判別を繰り返す必要がない。また、`preserving` は、部分式が変更するレジスタと後続のコードが必要とするレジスタの情報を組み合わせて、保存が必要かどうかを決められる。

評価器でも、式をあらかじめ解析して結果を再利用すれば、こうした繰り返しを減らせる。ただし、それは4.1.7節の解析と実行を分離した評価器や、翻訳系に近い仕組みを導入することになる。したがって、単に実行時の特別扱いを増やすだけで、翻訳系の利点をすべて除けるという主張には同意しない。
