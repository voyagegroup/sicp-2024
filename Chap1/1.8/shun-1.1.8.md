立方根をとるNewton法はyがxの立方根の近似値なら, よりよい近似は

(x/y^2+2y)/3

の値で与えられるという事実によっている. この式を使い平方根の手続きと似た立方根の手続きを実装せよ. (1.3.4節で平方根と立方根の手続きの抽象化として, 一般的なNewton法の実装法を学ぶ.)


```lisp
((define approximation guess x)
(/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
((define good-enough? guess pre-guess)
(< (abs (- guess pre-guess)) 0.001))

((define cube-iter guess pre-guess x)
(if (good-enough? guess pre-guess))
guess
(cube-iter (approximation guess x) guess x))

((define cube x)
(cube-iter 1.0 0 x))
```