問題 5.24

condをifへは簡約せず, 新しい基本的特殊形式として実装せよ. cond節の述語を順に, 真のものを見つけるまでテストするループを構成し, 次にev-sequenceを使い, その節の行動を評価しなければならない.

diff: https://github.com/voyagegroup/sicp-2024/pull/270/commits/da69b3c9abb87dcfd5af20b53896077b8c6bbd81

```
(cond ((= 1 2) 100)
      ((= 2 2) 200)
      (else 300))
```

```
(cond ((= 1 2) 100)
      ((= 3 2) 200)
      (else 300))
```


