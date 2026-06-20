```
(sum ?amount
     (and (job ?x (computer programmer))
          (salary ?x ?amount)))
```

このようなケースでは、?xに重複がないので問題ない。
ただし、wheelのように、同じ人物が複数の経路から導出される場合にはうまく働かない。

```
(sum ?amount
     (and (wheel ?x)
          (salary ?x ?amount)))
```

このとき、
```
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
```

Oliverを4回足し込むことになり、正確な計算ができない。

解決策としては、ストリームに対して人物などユニークなものに対して重複排除の処理をかけた上でsum等の処理を行う形にする必要がある。
