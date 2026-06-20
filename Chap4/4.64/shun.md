```racket

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))


; 実行
(outranked-by (Bitdiddle Ben) ?who)

; 本体
(or (supervisor (Bitdiddle Ben) ?who)
          (and (outranked-by ?middle-manager ?who)
               (supervisor (Bitdiddle Ben) ?middle-manager)))
```

実行した時の本体部分について、orはフレームのストリームを並列に処理するためどちらのストリームも実行される。
片方の`(supervisor (Bitdiddle Ben) ?who)`では、`(supervisor (Bitdiddle Ben) (Warbucks Oliver))`とマッチするため、それが表示される。
andは、`(outranked-by ?middle-manager ?who)`にマッチするすべての事項を見つけ、その結果のストリームの各フレームに対して`(supervisor (Bitdiddle Ben) ?middle-manager)`に当てはまるものを見つける。
束縛のないまま再帰的な`(outranked-by ?middle-manager ?who)`を実行すれば、探索が終わることはない。
よって、無限ループする。


元のoutranked-byは、supervisorで有限に絞ったストリームに対し、?middle-managerが束縛された状態で実行されるため、処理が適切に行われた。

```racket
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
```