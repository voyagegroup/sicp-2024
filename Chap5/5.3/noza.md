# 5.3 解答

## データパス

![./noza.png]()

## 制御器の定義


### 展開前

```
(controller
  (assign guess (const 1.0))

 test-guess
  (test (op good-enough?) (reg guess) (reg x))
  (branch (label sqrt-done))
  (assign guess
          (op improve)
          (reg guess)
          (reg x))
  (goto (label test-guess))

 sqrt-done)
 ```

### 展開した版

```
(controller
  (assign guess (const 1.0))

 test-guess
  (assign t
          (op *)
          (reg guess)
          (reg guess))
  (assign t
          (op -)
          (reg t)
          (reg x))
  (assign t
          (op abs)
          (reg t))
  (test (op <)
        (reg t)
        (const 0.001))
  (branch (label sqrt-done))

  (assign t
          (op /)
          (reg x)
          (reg guess))
  (assign t
          (op +)
          (reg guess)
          (reg t))
  (assign guess
          (op /)
          (reg t)
          (const 2))

  (goto (label test-guess))

 sqrt-done)
```

