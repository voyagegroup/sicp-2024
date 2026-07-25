# 解答

`reverse` は、空リストの場合と、先頭要素と残りのリストに分けられる場合で考える。

空リストを逆順にした結果は空リストである。

```scheme
(rule (reverse () ()))
```

空でないリストについては、リストを `(?first . ?rest)` に分ける。
`?rest` を逆順にしたものを `?reversed-rest` とし、その末尾に `?first` だけからなるリストを追加すれば、全体を逆順にしたリストになる。

```scheme
(rule (reverse (?first . ?rest) ?reversed)
      (and (reverse ?rest ?reversed-rest)
           (append-to-form ?reversed-rest
                           (?first)
                           ?reversed)))
```

例えば、

```scheme
(reverse (1 2 3) ?x)
```

を考えると、まず `(1 2 3)` を `1` と `(2 3)` に分ける。
次に `(2 3)` を逆順にし、さらに `(3)` を逆順にする。
最後に、順に末尾へ追加していくことで、`?x` は `(3 2 1)` になる。

したがって、この規則は次の質問には答えられる。

```scheme
(reverse (1 2 3) ?x)
```

結果は、

```scheme
(reverse (1 2 3) (3 2 1))
```

である。

一方で、

```scheme
(reverse ?x (1 2 3))
```

のように第一引数が未束縛の場合、この規則ではうまく答えられない。
なぜなら、規則の本体では先に

```scheme
(reverse ?rest ?reversed-rest)
```

を評価するため、`?rest` も `?reversed-rest` も十分に制約されないまま再帰が進んでしまうからである。
その結果、探索が無限に広がり、`append-to-form` によって第二引数 `(1 2 3)` から逆算するところまで到達しにくい。

つまり、この規則は論理的には `reverse` の関係を表しているが、質問システムの評価順序のため、実用上は

```scheme
(reverse (1 2 3) ?x)
```

の方向には答えられても、

```scheme
(reverse ?x (1 2 3))
```

の方向には答えられない。
ここでも、論理的な関係としては対称的に見えても、手続き的な評価順序によって動作が変わることが分かる。
