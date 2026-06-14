# 問題 4.61

## ルール定義

```scheme
(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))
```

1つ目のルール: リストの先頭2要素が `?x`, `?y` なら成立。  
2つ目のルール: 先頭をスキップして残りのリストで再帰的に探す。

---

## クエリ 1

```scheme
(?x next-to ?y in (1 (2 3) 4))
```

**結果:**

```
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))
```

---

## クエリ 2

```scheme
(?x next-to 1 in (2 1 3 1))
```

**結果:**

```
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))
```
