```
(define (list-of-values exps env)
  (display exps)

; でexpsの中身をみてみる

;;; M-Eval input:
(cons 1 2)

(1 2)

(2)

()
```

のように表示された

これが、1 → 2なのか、 2 → 1で処理されるかの話。

前似たようなことをした、問3.8を使えば確認ができそう。

- https://sicp.iijlab.net/fulltext/x313.html
- https://github.com/voyagegroup/sicp-2024/blob/main/Chap3/3.8/ryu-3_8.rkt

```
(define f
  ((lambda (first-time?)
     (lambda (x)
       (if first-time?
           (begin
             (set! first-time? false)
             x)
           0)))
   true))

(+ (f 0) (f 1))

; 左から（pから処理）
; (f 0) -> 0
; (f 1) -> 0

; 右から（1から処理）
; (f 1) -> 1
; (f 0) -> 0
```


