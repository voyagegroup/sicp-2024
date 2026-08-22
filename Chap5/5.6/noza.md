# 問題5.6解答

afterfib-n-1 の restore と save が考えられる。
元の制御列は以下。

```
afterfib-n-1
  (restore n)
  (restore continue) ; --- (1)
  ;; Fib(n-2)を計算するよう設定
  (assign n (op -) (reg n) (const 2))
  (save continue) ; --- (2)
  (assign continue (label afterfib-n-2))
  (save val)
  (goto (label fib-loop))
```

(1) で取り出した直後に (2) ですぐに戻している。
Fib(n-2) の計算中も、元の continue の値をスタックに残しておけば良いので、この2つの命令が不要になる。
