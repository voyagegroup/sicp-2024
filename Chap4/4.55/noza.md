# 問題 4.55

データベースに対して以下のクエリを実行する。

---

## a. Ben Bitdiddle に直属する全従業員

```scheme
(supervisor ?x (Bitdiddle Ben))
```

**結果:**

```
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
```

---

## b. 会計部門の全従業員の名前

```scheme
(job ?x (accounting . ?type))
```

**結果:**

```
(job (Scrooge Eben) (accounting chief accountant))
(job (Cratchet Robert) (accounting scrivener))
```

---

## c. Slumerville 在住の全従業員の名前

```scheme
(address ?x (Slumerville . ?rest))
```

**結果:**

```
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(address (Aull DeWitt) (Slumerville (Onion Square) 5))
```
