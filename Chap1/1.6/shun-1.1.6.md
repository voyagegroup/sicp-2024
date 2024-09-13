> (define (new-if predicate then-clause else-clause)
>  (cond (predicate then-clause)
>        (else else-clause)))

> (define (average x y)
>  (/ (+ x y) 2))

> (define (improve guess x)
> (average guess (/ x guess)))

> (define (good-enough? guess x)
> (< (abs (- (square guess) x)) 0.001))

> (define (sqrt-iter guess x)
>   (new-if (good-enough? guess x)
>           guess
>           (sqrt-iter (improve guess x) x)
> ))
> Alyssaが平方根を計算するのにこれを使おうとすると, 何が起きるか, 説明せよ.

Interactions disabled; out of memory
無限ループに陥ってしまい、答えが出せない。
これはnew-ifが通常の手続きであるため、引数を先に評価しようとするためである。
guessが3でxが9の時を考える。このときgood-enough?は真である。
ただ、作用的順序評価では引数が先に評価される。
つまり、new-if関数の引数にあるsqrt-iterが呼ばれてしまうのである。
このため、条件式が真であっても再起を行い続けてしまうため、無限ループになる。

