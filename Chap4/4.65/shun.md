```
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
(supervisor (Reasoner Louis) (Hacker Alyssa P))
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(supervisor (Scrooge Eben) (Warbucks Oliver))
(supervisor (Cratchet Robert) (Scrooge Eben))
(supervisor (Aull DeWitt) (Warbucks Oliver))
```

```
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))
```

この規則は、ある人(?person)に部下(?middle-manager)がいて、その部下が誰か(?x)の上司であることを定義している。

`(wheel ?who)`と実行すると、すべての関係性から規則に当てはまる人物を取り出す。
それに当てはまるのが以下の5人で、Oliverの部下を上司にもつものたちが4人いるため4回出力された。

```
(Hacker Alyssa P) → (Bitdiddle Ben)　→ (Warbucks Oliver)
(Fect Cy D) → (Bitdiddle Ben) → (Warbucks Oliver)
(Tweakit Lem E) → (Bitdiddle Ben) → (Warbucks Oliver)
(Reasoner Louis) → (Hacker Alyssa P) → (Bitdiddle Ben)
(Cratchet Robert) → (Scrooge Eben) → (Warbucks Oliver)
```

Oliverに直属の場合は`(supervisor ?x ?middle-manager)`に当てはまらず、表示されない。

```
(Bitdiddle Ben) → (Warbucks Oliver) → x
(Scrooge Eben) → (Warbucks Oliver) → x
(Aull DeWitt) → (Warbucks Oliver) → x
```