```
(assert! (rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2)))))

(assert! (rule (same ?x ?x)))
```

person-1にもperson-2にも全ての人が入りうる。そして、順序が入れ替わることを禁止するルールはないため2度登場する。
文字列の順序比較ができれば順序を固定できる。

```
(and (lives-near ?person-1 ?person-2)
     (lisp-value < ?person-1 ?person-2))
```