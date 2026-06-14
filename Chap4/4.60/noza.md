# 問題 4.60

## なぜ重複が起きるか

`lives-near` ルール：

```scheme
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))
```

このルールは **対称的** なので、`?person-1` と `?person-2` を入れ替えても成立する。  
`(lives-near ?person-1 ?person-2)` と問い合わせると両方向が返ってくる：

```
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))
```

## 重複を避ける方法

`lisp-value` で名前に順序付けをして、片方向だけ通すようにする。  
たとえば姓（`car` の部分）をアルファベット順で比較する：

```scheme
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (lisp-value string<?
                       (symbol->string (car ?person-1))
                       (symbol->string (car ?person-2)))))
```

`person-1` の姓が `person-2` の姓より前に来る組み合わせだけを通すことで、各ペアが一方向のみ現れるようになる。  
（`not (same ...)` も不要になる）
