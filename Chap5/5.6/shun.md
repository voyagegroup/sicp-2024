afterfib-n-1内にある`(restore continue)`と`(save continue)`はstackから出して戻すだけで不要。

```
afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
   (restore n)
   (restore continue)
   ;; Fib(n-2)を計算するよう設定
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; Fib(n-1)を退避
   (goto (label fib-loop))
```