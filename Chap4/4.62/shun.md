```
(assert! (rule (last-pair (?x) (?x))))

(assert! (rule (last-pair (?li . ?rest) ?last)
      (last-pair ?rest ?last)))
```

```
;;; Query input:
(last-pair (3) ?x)

;;; Query results:
(last-pair (3) (3))

;;; Query input:
(last-pair (1 2 3) ?x)

;;; Query results:
(last-pair (1 2 3) (3))

;;; Query input:
(last-pair (2 ?x) (3))

;;; Query results:
(last-pair (2 3) (3))

```

以下の解が無限にある問い合わせについては答えを出さない。

```
;;; Query input:
(last-pair ?x (3))
```