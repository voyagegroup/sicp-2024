# 問題 4.57

person-1 が person-2 を置き換えられる条件：
- person-1 の職種が person-2 の職種と同じ、または `can-do-job` で対応できる
- person-1 と person-2 が同一人物でない

## ルール定義

同一性チェック用の補助ルール（教科書でも使われる）：

```scheme
(rule (same ?x ?x))
```

`can-replace` ルール：

```scheme
(rule (can-replace ?person-1 ?person-2)
      (and (job ?person-1 ?job-1)
           (job ?person-2 ?job-2)
           (or (same ?job-1 ?job-2)
               (can-do-job ?job-1 ?job-2))
           (not (same ?person-1 ?person-2))))
```

---

## a. Cy D. Fect を置き換えられる人

```scheme
(can-replace ?person (Fect Cy D))
```

**結果:**

```
(can-replace (Bitdiddle Ben) (Fect Cy D))
(can-replace (Hacker Alyssa P) (Fect Cy D))
```

- Ben: `(can-do-job (computer wizard) (computer programmer))` が成立
- Hacker: 同じ職種 `(computer programmer)`

---

## b. 置き換え可能な全ての組み合わせ

```scheme
(can-replace ?person-1 ?person-2)
```

**結果:**

```
(can-replace (Bitdiddle Ben) (Hacker Alyssa P))
(can-replace (Bitdiddle Ben) (Fect Cy D))
(can-replace (Bitdiddle Ben) (Tweakit Lem E))
(can-replace (Hacker Alyssa P) (Fect Cy D))
(can-replace (Hacker Alyssa P) (Reasoner Louis))
(can-replace (Fect Cy D) (Hacker Alyssa P))
(can-replace (Fect Cy D) (Reasoner Louis))
(can-replace (Aull DeWitt) (Warbucks Oliver))
```
