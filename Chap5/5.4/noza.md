# 問題5.4 解答

## 再帰的べき乗

```sql
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
```

必要なレジスタは以下
- b: 底
- n: 指数
- val: 計算結果
- continue: 再起呼び出しからの戻り先

再起呼び出しのために stack に保存すべきは以下
- continue: 再起処理の戻る箇所を覚えておく必要がある

### 制御の流れ

```
(controller
  (assign continue (label expt-done))

 expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))

  (save continue)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-expt))
  (goto (label expt-loop))

 after-expt
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))

 base-case
  (assign val (const 1))
  (goto (reg continue))

 expt-done)
 ```

## 反復的べき乗

```
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))
```

必要なレジスタは以下
- b: 底
- counter: 残りの乗算回数
- product: 現在までの計算結果

### 制御の流れ

```
(controller
  (assign counter (reg n))
  (assign product (const 1))

 expt-iter
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))

  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg b) (reg product))
  (goto (label expt-iter))

 expt-done)
 ```
