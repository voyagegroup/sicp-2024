a. Ben Bitdiddleが監督している人すべての名前とその住所;

```
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?where))
```

b. 給料がBen Bitdiddleのそれより少ない人のすべてと, その人たちの給料と, Ben Bitdiddleの給料;

```
(and (salary ?person ?amount)
     (salary (Bitdiddle Ben) ?ben)
     (lisp-value < ?amount ?ben))
```

c. 計算機部門にいない人が監督している人すべてと, その監督者の名前と担当.

```
(and (supervisor ?x ?manager)
     (not (job ?manager (computer . ?type)))
     (job ?manager ?j))
```