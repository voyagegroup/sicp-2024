```scheme
#lang sicp

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(try try)
```

これを評価する。
実行部を置き換えるとこのようになる。

```scheme
(if (halts? try try)
      (run-forever)
      'halted)
```

もしhalts?がtrue,つまり停止すると考える場合(run-forever)が実行される。
しかし、(run-forever)が実行されるということは永遠に走るということだ。
それではtryは停止するとは言えないため、これは矛盾する。

halts?がfalse,停止しないと考える場合'haltedが返却される。
値が返却されるということは停止するということなのでこれも矛盾する。

よってhalts?は成立せず、手続きとして書くことはできない。
