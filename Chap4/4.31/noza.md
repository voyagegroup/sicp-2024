# 問題 4.31 解答

実装は [`noza.rkt`](/Users/noza/go/src/github.com/voyagegroup/sicp-2024/Chap4/4.31/noza.rkt) にある。

## 方針

元の遅延評価器では、合成手続きの引数はすべて一律に「遅延 + メモ化」されていた。  
この問題では各仮引数ごとに次の 3 種類を指定できるようにした。

- `x` : 通常の strict 引数
- `(x lazy)` : 遅延するがメモ化しない
- `(x lazy-memo)` : 遅延し、最初の強制結果をメモ化する

例えば

```scheme
(define (f a (b lazy) c (d lazy-memo))
  ...)
```

では、

- `a` と `c` は呼び出し時に評価される
- `b` は必要になった時に毎回再評価される
- `d` は必要になった時に評価され、その後は結果を再利用する

## 実装上の変更

### 1. 仮引数宣言の解析

仮引数は

- 記号なら `strict`
- 2 要素リスト `(name mode)` なら `lazy` または `lazy-memo`

として解釈するようにした。

主な手続き:

- `parameter-declaration?`
- `parameter-name`
- `parameter-mode`
- `parameter-names`

### 2. `apply` の変更

合成手続きの適用時に、仮引数ごとの mode を見て実引数を次のように束縛する。

- `strict` : `actual-value`
- `lazy` : `delay-it`
- `lazy-memo` : `delay-memo-it`

そのため `my-apply` では `list-of-delayed-args` の代わりに `list-of-parameter-args` を使うようにした。

### 3. thunk の種類を 2 つに分離

元の評価器では thunk はすべてメモ化されていた。  
4.31 では次の 2 種類を区別する。

- `thunk` : 非メモ化
- `memo-thunk` : メモ化あり

`force-it` は

- `thunk` なら毎回再評価
- `memo-thunk` なら最初の 1 回だけ評価し、`evaluated-thunk` に書き換え
- `evaluated-thunk` なら保存値を返す

という分岐にした。

## 動作確認

次の入力で確認した。

```scheme
(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define (ignore x (y lazy) (z lazy-memo)) x)
(ignore (id 10) (id 20) (id 30))
count

(set! count 0)
(define (use-lazy (x lazy)) (+ x x))
(use-lazy (id 10))
count

(set! count 0)
(define (use-memo (x lazy-memo)) (+ x x))
(use-memo (id 10))
count
```

結果:

- `ignore` の後の `count` は `1`
  - strict な `x` だけが呼び出し時に評価される
- `use-lazy` の後の `count` は `2`
  - `x` を 2 回使うので毎回再評価される
- `use-memo` の後の `count` は `1`
  - 最初の強制結果が再利用される
