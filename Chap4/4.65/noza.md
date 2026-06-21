# 解答

4.4.1 より `Oliver` は以下の人物を部下に持ち、それぞれの人物もまた部下を持つ

- Ben
  - Alyssa
  - Fect Cy D
  - Lem
- Eben
  - Robert
- Dewitt

そのため、

```scheme
(wheel ?who)
```

?who=Oliver の場合に満たすフレームは 4 つ存在する。
質問システムには重複を排除する機能はないため条件を満たす 4 つが出力された。
