## 階乗

```
(factorial 3)
; 初期
n = 3
continue = fact-done
val = ?
stack = ()

; save1回目
stack = (3 fact-done)

(factorial 2)
; save2回目
stack = (2 after-fact 3 fact-done)

(factorial 1)
base-case
val = 1

; after-fact1回目
restore
stack = (3 fact-done)
val = 2

; after-fact2回目
restore
stack = ()
val = 6

fact-done
```

## fibonacci

```
(fibonacci 3)
stack = (3 fib-done)
continue = afterfib-n-1

(fibonacci 2)
continue = afterfib-n-1
stack = (2 afterfib-n-1 3 fib-done)

(fibonacci 1)
immediate-answer
val = 1

afterfib-n-1
n = 2
stack = (3 fib-done)
n = 0
stack = (afterfib-n-1 3 fib-done)
continue = afterfib-n-2
stack = (1 afterfib-n-1 3 fib-done)

(fibonacci 0)
val = 0

afterfib-n-2
val = 1
continue = afterfib-n-1
stack = (3 fib-done)
val = 1

afterfib-n-1
n = 3
continue = fib-done
n = 1
continue = afterfib-n-2
stack = (1 fib-done)

fib-loop
immediate-answer
val = 1

afterfib-n-2
n = 1
val = 1
continue = fib-done
val = 2

fib-done
```