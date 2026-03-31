# 問題 4.26 解答

## Ben と Alyssa の主張

Ben の言う通り、作用的順序の言語でも `unless` を**特殊形式**として実装すれば、不要な枝を評価しない制御構造として使える。

一方で Alyssa の言う通り、そのような `unless` は**第一級の手続きではなく構文**になるため、高階手続きと組み合わせて普通の手続きのようには使えない。

したがって両者は矛盾しているのではなく、見ている点が違う。

- Ben は「作用的順序でも `unless` 的な制御構造は実装できる」と言っている。
- Alyssa は「しかしそれは手続きとしての一般性を失う」と言っている。

## `unless` の導出された式としての実装

`unless` は `cond` や `let` と同様に、表面構文を既存の基本構文へ変換する**導出された式**として実装できる。

例えば

```scheme
(unless condition usual-value exceptional-value)
```

を

```scheme
(if condition exceptional-value usual-value)
```

へ変換すればよい。

これは意味的には「`condition` が真なら例外側を返し、偽なら通常側を返す」という `unless` の意図と一致する。

したがって作用的順序の処理系でも、`unless` を新しい評価規則を持つ構文として導入すれば、4.25 のような問題は避けられる。

## Alyssa の反論の意味

ただし、上の `unless` は**手続きではない**。

手続きであれば

- 変数に束縛できる
- 引数として渡せる
- 戻り値にできる
- データ構造に入れられる

が、特殊形式や導出された式としての `unless` はそうではない。

`if` と同様に、`(unless ...)` という構文の形でしか使えず、「値としての手続き」として扱うことはできない。

この意味で Alyssa は、「誰かが `unless` を特殊形式として実装したら、それは高水準手続きと組み合わせて使える手続きではなく、単なる構文になる」と反論している。

## 手続きとして使えると有用な状況

`unless` が手続きなら、条件分岐の仕方そのものを引数として受け取るような高階手続きに渡せる。

例えば概念的には次のような使い方を考えられる。

```scheme
(define (select chooser condition usual exceptional)
  (chooser condition usual exceptional))
```

ここで `chooser` が第一級の手続きなら、`if` 的なものや `unless` 的なものを値として渡せる。

```scheme
(select unless (= b 0)
        (/ a b)
        0)
```

手続き版 `unless` なら、このように「条件分岐の演算子」を他の手続きへ渡して抽象化できる。

しかし特殊形式版 `unless` は値ではないので、`chooser` の位置に渡すことができない。`map` や `fold` のように「手続きを受け取る高階手続き」とも同じ意味では組み合わせられない。

## どう評価されるか

### 通常の手続きとして定義

```scheme
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
```

と**普通の手続き**として定義すると、`(unless c u e)` の評価は次のようになる。

1. `unless` という演算子名を環境から引いて、手続きオブジェクトを得る
2. `c`, `u`, `e` を `list-of-values` で全部評価する
3. その後で、ようやく `unless` の本体 `(if condition exceptional-value usual-value)` を実行する

つまり、`if` に入る前に `usual-value` と `exceptional-value` の両方がすでに評価済みなので、4.25 の問題が起きる。

これは `unless` が合成手続きとして `make-procedure` で作られ、他の手続きと同じく `my-apply` に渡されるからである。手続きとして扱われる以上、評価器は「引数を評価してから適用する」という通常の規則を変えない。

### 特殊構文にすると何が変わるか

一方、`cond` や `let` と同じように `unless` を**導出された式**として実装するなら、`eval` に

```scheme
((unless? exp) (eval (unless->if exp) env))
```

のような分岐を追加することになる。

すると `unless` 式は手続き適用として `my-apply` に渡される前に、

```scheme
(unless c u e)
```

から

```scheme
(if c e u)
```

へ変換される。あとは既存の `eval-if` が

- 述語 `c` を評価する
- 真なら `e` だけ評価する
- 偽なら `u` だけ評価する

ので、不要な枝は評価されない。

`Chap4/4.22/noza.rkt` の analyze 型評価器でも考え方は同じで、`let` を

```scheme
((let? exp) (analyze (let->combination exp)))
```

としているのと同様に、`unless` も analyze 段階で `if` に変換すればよい。重要なのは、**手続き適用の経路に乗せないこと**である。

### なぜ第一級の手続きではなくなるのか

特殊構文版 `unless` は、環境の中に値として入っている手続きではない。

普通の手続きなら `lambda` を評価したとき `make-procedure` により

```scheme
(procedure parameters body env)
```

のような手続きオブジェクトが作られ、変数に束縛できる。だから

- 変数に代入する
- 引数として渡す
- 戻り値にする

が可能になる。

しかし特殊構文版 `unless` は、`eval` が「式の先頭が `unless` か」を見て分岐するための**構文上の印**にすぎない。`unless` という名前の値が環境に束縛されているわけではない。

そのため、例えば

```scheme
(define chooser unless)
```

の右辺の `unless` は「構文」ではなく単なる変数参照として扱われる。ところが環境には `unless` という手続き値がないので、手続きとして取り出すことができない。

同様に

```scheme
(select unless condition usual exceptional)
```

の `unless` も、手続き適用の被演算子として現れた時点では単なる変数であり、第一級の手続き値ではない。したがって高階手続きへ渡せない。

要するに、特殊構文にした `unless` は「`eval` が特別扱いする式の形」であって、「環境の中を流通できる値」ではない。これが、特殊構文にすると第一級の手続きではなくなる、という意味である。

## 結論

- Ben は正しい。`unless` は作用的順序でも特殊形式、あるいは導出された式として実装できる。
- Alyssa も正しい。その実装では `unless` は第一級の手続きではなくなり、高階手続きと組み合わせる一般性を失う。

したがってこの論争の本質は、「`unless` を実装できるか」ではなく、「`unless` を構文として導入するだけで十分か、それとも手続きとして使えることに価値があるか」である。
