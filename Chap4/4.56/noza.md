# 問題 4.56

複合クエリを使ってデータベースを検索する。

## a. Ben Bitdiddle の部下全員の名前と職務

```scheme
(and (supervisor ?person (Bitdiddle Ben))
     (job ?person ?job))
```

## b. Ben より給与が低い全従業員と、その人の給与と・Ben の給与

```scheme
(and (salary (Bitdiddle Ben) ?ben-salary)
     (salary ?person ?amount)
     (lisp-value < ?amount ?ben-salary))
```

## c. コンピュータ部門でない従業員と、その上司の名前

```scheme
(and (supervisor ?person ?boss)
     (not (job ?person (computer . ?type))))
```
