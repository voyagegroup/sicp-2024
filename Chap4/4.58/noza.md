# 問題 4.58

ある部門の「big-shot」を定義する。  
big-shot の条件：その部門で働いていて、かつ同じ部門に自分の上司がいない。

## ルール定義

```scheme
(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?type))
           (not (and (supervisor ?person ?boss)
                     (job ?boss (?division . ?boss-type))))))
```

