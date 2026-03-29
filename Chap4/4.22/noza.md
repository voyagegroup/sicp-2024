# 問題 4.22 解答

## `let` 対応

let を導出された式として lambda に変換した上で analyze すればよい

```scheme
((let? exp) (analyze (let->combination exp)))
```

`let->combination` は 4.6 と同様に `((lambda (...) ...) ...)` へ変換する。
