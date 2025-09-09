## appendの定義

> ppendのように, リストを cdrダウンしつつ, 「consアップ」するものである

## appendの挙動

```
(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

z
; (a b c d)

(cdr x)
; (b)

x
; (a b)

; x のcdrを書き換える。
; もし、appendで生成されたzのxが同じポインタを見ている場合、(a aaa c d)になるはず
(set-cdr! x (list 'aaa))

x
; (a aaa)

(cdr x)
; (aaa)

z
; (a b c d)'))
```

