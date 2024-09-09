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

2の平方根を求めるとする。
guessを1、xを2とする。
まず、new-ifの条件が評価される。
これは偽になる。
よって、(sqrt-iter (improve guess x) x)が実行される。
こうして再起的にsqrt-iterが呼び出されて、(good-enough? guess x)が真になった時点で、guessが返されて実行が終わる。


https://fatima.adingo.jp/timeline/073d18d1-9428-4cc1-a1f9-b46a39d436c0