## fibonacci数の計算


```scheme
(define test-program
  '(begin
     (define (fib n)
       (if (< n 2)
           n
           (+ (fib (- n 1)) (fib (- n 2)))))
     (fib 30)))
```

### 元の版

```scheme
(define (run-original-eval)
  (let ((start-time (runtime)))
    (eval test-program the-global-environment)
    (let ((end-time (runtime)))
      (display "Original Eval Time: ")
      (display (- end-time start-time))
      (newline))))

(run-original-eval)
```

### analyze版

```scheme
(define (run-analyzing-eval)
  (let ((start-time (runtime)))
    ;; test-program を新しい eval (analyzeを使う版) で評価
    (eval test-program the-global-environment)
    (let ((end-time (runtime)))
      (display "Analyzing Eval Time: ")
      (display (- end-time start-time))
      (newline))))

(run-analyzing-eval)
```

## 結果

回数 | 元の版 | analyze版
-- | -- | --
25 | 737,693 | 471,685
30 | 8,182,844 | 5,828,688

解析が一度で済む分、約1.5~2倍ほど早い。