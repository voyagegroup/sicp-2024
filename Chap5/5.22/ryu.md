# 5.22

append

> appendはxの要素を次々とyへconsして新しいリストを作る.

https://sicp.iijlab.net/fulltext/x331.html

```
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define x (list 1 2 3))
(define y (list 4 5))

(append x y)
; (1 2 3 4 5)

```

append!

> 手続きappend!はappendに似ているが, 構成子ではなく変更子である. xの最後の対を, そのcdrがyになるように修正し, リストを一緒に張り合せて連結する. (空のxについてappend!を呼び出すとエラーになる.)


```
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

```



