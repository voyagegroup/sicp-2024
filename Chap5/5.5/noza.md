# 問題5.5解答

## 階乗

元の定義

```
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
```

制御

```
(controller
   (assign continue (label fact-done))     ; 最終帰り番地設定
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;;nとcontinueを退避し再帰呼出しを設定する.
   ;; 再帰呼出しから戻る時after-fact}から
   ;; 計算が続行するようにcontinueを設定
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; valに n(n-1)!がある
   (goto (reg continue))                   ; 呼出し側に戻る
 base-case
   (assign val (const 1))                  ; 基底の場合: 1!=1
   (goto (reg continue))                   ; 呼出し側に戻る
 fact-done)
```

n = 3 から始める。

```
n = 3
continue = fact-done
val = 未定義
stack = []
```

再帰呼び出し

```
n = 2
continue = after-fact
val = 未定義
stack = [3, fact-done]
```

再帰呼び出し

```
n = 1
continue = after-fact
val = 未定義
stack = [2, after-fact, 3, fact-done]
```

n == 1 なので、base-case へ

```
n = 1
continue = after-fact
val = 1
stack = [2, after-fact, 3, fact-done]
```

ここから stack の restore 操作

```
n = 2
continue = after-fact
val = 2
stack = [3, fact-done]
```

continue が after-fact なので after-fact へ

```
n = 3
continue = fact-done
val = 6
stack = []
```

continue が fact-done なので fact-done　へ
終了

## Fibnacci

元の定義

```
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

制御

```
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; Fib(n-1)を計算するよう設定
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; nの昔の値を退避
   (assign n (op -) (reg n) (const 1)); nを n-1 に変える
   (goto (label fib-loop))            ; 再帰呼出しを実行
 afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
   (restore n)
   (restore continue)
   ;; Fib(n-2)を計算するよう設定
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; Fib(n-1)を退避
   (goto (label fib-loop))
 afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
   (assign n (reg val))               ; nにはFib(n-2)がある
   (restore val)                      ; valにはFib(n-1)がある
   (restore continue)
   (assign val                        ; Fib(n-1)+Fib(n-2)
           (op +) (reg val) (reg n))
   (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
 immediate-answer
   (assign val (reg n))               ; 基底の場合: Fib(n)=n
   (goto (reg continue))
 fib-done)
```

n = 3 の時

```
n = 3
continue = fib-done
val = 未定義
stack = []
```

Fib(2) の計算

```
n = 2
continue = after-fib-n-1
val = 未定義
stack = [3, fib-done]
```

Fib(1) の計算

```
n = 1
continue after-fib-n-1
val = 未定義
stack = [2, after-fib-n-1, 3, fib-done]
```

n = 1 なので immediate-answer にジャンプ

```
n = 1
continue = after-fib-n-1
val = 1
stack = [2, after-fib-n-1, 3, fib-done]
```

(goto (reg continue)) により after-fib-n-1 にジャンプ

```
n = 2
continue = after-fib-n-1
val = 1
stack = [3, fib-done]
```

Fib(1) の結果を残しておく必要があるので save

```
n = 0
continue = after-fib-n-2
val = 1
stack = [1, after-fib-n-1, 3, fib-done]
```

Fib(0) を計算。fib-loop に戻り、immediate-answer にジャンプ。

```
n = 0
continue = after-fib-n-2
val = 0
stack = [1, after-fib-n-1, 3, fib-done]
```

after-fib-n-2 にジャンプ。

```
n = 0
continue = after-fib-n-1
val = 1
stack = [3, fib-done]
```

after-fib-n-1 にジャンプ。

```
n = 1
continue = after-fib-n-2
val = 1
stack = [1, fib-done]
```

fib-loop にジャンプ。n < 2 なので、immediate-answer にジャンプ

```
n = 1
continue = after-fib-n-2
val = 1
stack = [1, fib-done]
```

after-fib-n-2 にジャンプ

```
n = 1
continue = fib-done
val = 2
stack = []
```

fib-done ジャンプ。

結果が val = 2
