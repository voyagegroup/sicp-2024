# 解答

孫の関係を、`grandson` だけでなく、`great-grandson` や `great-great-grandson` のように一般化する。
問題文のヒントに合わせて、関係名はリストとして表す。

例えば、Adam から見て Irad が孫の子であることは、次のような形で表す。

```scheme
((great grandson) Adam Irad)
```

規則は以下。

```scheme
(rule (ends-in-grandson (grandson)))

(rule (ends-in-grandson (?x . ?rest))
      (ends-in-grandson ?rest))

(rule ((grandson) ?g ?s)
      (and (son ?g ?f)
           (son ?f ?s)))

(rule ((great . ?rel) ?x ?y)
      (and (ends-in-grandson ?rel)
           (son ?x ?z)
           (?rel ?z ?y)))
```
