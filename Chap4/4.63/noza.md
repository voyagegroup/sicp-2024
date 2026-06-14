# 問題 4.63

## データベース

```scheme
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)
```

---

## ルール定義

### grandson（孫）

```scheme
(rule (grandson ?grandfather ?grandson)
      (and (son ?grandfather ?father)
           (son ?father ?grandson)))
```

### son の拡張（妻の息子を夫の息子としても扱う）

`(wife Lamech Ada)` + `(son Ada Jabal)` だけでは Lamech の息子が見えないため、妻経由の関係をカバーするルールを追加する。

```scheme
(rule (son ?man ?boy)
      (and (wife ?man ?woman)
           (son ?woman ?boy)))
```

