```racket
#lang sicp

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)

```

作用的順序の場合、引数は手続を作用する時に全て評価する。
`(factorial 1)`で止まることが期待されるが、その時も引数の`(factorial (- 1 1))`が評価されていくため終了することなくout of memoryになる。

正規順序の場合は、引数の値が必要になったときに評価されるため、終了条件を満たした時にfactorialが呼ばれることはなく終了する。