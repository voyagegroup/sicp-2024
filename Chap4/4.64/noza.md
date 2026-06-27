# 4.64 解答

## それぞれの実装

元の実装

```scheme
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
```

修正した実装

```scheme
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))
```

### 実装の違い

最後の and の順序が異なる

```scheme
; 前のバージョン
(and (supervisor ?staff-person ?middle-manager)
     (outranked-by ?middle-manager ?boss))

; 修正したバージョン
(and (outranked-by ?middle-manager ?boss)
     (supervisor ?staff-person ?middle-manager))
```

## 何が起こるか

以下の質問をする

```scheme
(outranked-by (Bitdiddle Ben) ?who)
```

これにより作成されるフレームは、

```
{
    ?staff-person = Ben
    ?boss = ?who
}
```

修正前のバージョンでは、上記フレームに対してまず

```scheme
(supervisor ?staff-person ?middle-manager)
```

を展開するので

```scheme
(supervisor (Bitdiddle Ben) ?middle-manager)
```

となり、Ben の supervisor を取得したフレームが拡張され他後に、

```scheme
(outranked-by ?middle-manager ?boss)
```

が展開される。

修正後のバージョンでは、

```
(outranked-by ?middle-manager ?boss)
```

を展開するので、

```scheme
(outranked-by ?middle-manager ?who)
```

?middle-manager がまだ束縛されていないため、
(outranked-by ?middle-manager ?who)
という一般的な問い合わせになり、
同じ規則の再帰節を再び適用できてしまう。
その結果、supervisor によって候補を絞る前に
outranked-by の再帰が繰り返され、無限ループになる。
