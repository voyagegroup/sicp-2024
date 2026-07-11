# 解答

`delay` を使わない定義では、ストリームの後半を生成する式が、先頭の答えを返す前に評価されてしまう。
そのため、後半の計算が無限再帰に入る場合、前半から得られるはずの答えも取り出せなくなる。

例えば、次のような自己再帰する規則をデータベースに追加する。

```scheme
(assert! (rule (loop ?x)
               (loop ?x)))
```

## `disjoin` の場合

次の質問を考える。

```scheme
(or (job (Bitdiddle Ben) ?job)
    (loop ?x))
```

本来の `disjoin` は次のように、残りの選言肢の評価を `delay` している。

```scheme
(interleave-delayed
 (qeval (first-disjunct disjuncts) frame-stream)
 (delay (disjoin (rest-disjuncts disjuncts)
                 frame-stream)))
```

そのため、まず

```scheme
(job (Bitdiddle Ben) ?job)
```

から得られる答えを返すことができる。第二の選言肢 `(loop ?x)` は、さらに答えが要求されるまで評価されない。

一方、Louis の定義では

```scheme
(interleave
 (qeval (first-disjunct disjuncts) frame-stream)
 (disjoin (rest-disjuncts disjuncts) frame-stream))
```

となっているため、Scheme の作用的順序評価により `interleave` を呼び出す前に第二引数が評価される。

その結果 `(loop ?x)` の評価が無限再帰に入り、第一の選言肢から得られるはずの答えも表示されない。

## `simple-query` の場合

次の表明と規則を追加する。

```scheme
(assert! (p a))

(assert! (rule (p ?x)
               (p ?x)))
```

そして

```scheme
(p ?x)
```

を質問する。

`find-assertions` は表明 `(p a)` とマッチするため、`?x = a` という答えを生成できる。

本来の `simple-query` では

```scheme
(stream-append-delayed
 (find-assertions query-pattern frame)
 (delay (apply-rules query-pattern frame)))
```

となっているため、まず表明から得られた答えを返し、その後で規則の適用を試みる。

しかし Louis の定義では `stream-append` を呼び出す前に

```scheme
(apply-rules query-pattern frame)
```

が評価される。

規則 `(p ?x) :- (p ?x)` は同じ質問を繰り返すため無限再帰となり、表明から得られる `?x = a` も返されなくなる。

以上より、`delay` の目的はストリームの後半の計算を必要になるまで遅延させ、後半に停止しない計算が含まれていても、前半から得られる答えを利用できるようにすることである
