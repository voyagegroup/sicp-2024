```scheme
(lambda ⟨vars⟩
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a ⟨e1⟩)
          (b ⟨e2⟩))
      (set! u a)
      (set! v b))
    ⟨e3⟩))
```

```scheme
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
```

問題文の定義で掃き出すと、手続きは以下のようになる。

```scheme
(lambda ⟨vars⟩
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))
```

letで定義すると、以下のようなlambdaになる。

```scheme
((lambda (a b)
   (set! y a)
   (set! dy b))
 (integral (delay dy) y0 dt)
 (stream-map f y))
```

このとき最小の部分式は、以下である。

```scheme
(integral (delay dy) y0 dt)
 (stream-map f y)
```

しかし、これを実行するときはまだyがset!されておらずunassignedである。
この状態でstream-mapが走ってもstreamになっていないためエラーになる。
よって動かない。


本文にある通りに掃き出すと以下のようになる。

```scheme
(lambda ⟨vars⟩
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
```

こちらではyがsetされてからdyのsetがされるため、問題なく動作する。
