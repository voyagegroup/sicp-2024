```
(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)))

(assert! (rule (reverse () ())))
      
(assert! (rule (reverse (?x . ?rest) ?result)
      (and (reverse ?rest ?rev-rest)
           (append-to-form ?rev-rest (?x) ?result))))
```

`(reverse (1 2 3) ?x)`には答えられるが、`(reverse ?x (1 2 3))`については再帰が無限に探索して結果が出ない。