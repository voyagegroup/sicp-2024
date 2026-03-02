# 問題 4.18 解答

内部定義を次の形に掃き出す戦略を考える。

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

solve:

```scheme
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
```

## 1. この問題の戦略で掃き出した場合

* u = y
* v = dy
* e1 = (integral (delay dy) y0 dt)
* e2 = (stream-map f y)

とすると、以下のように掃き出せる

```scheme
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))
```

ここで b を計算する時点では y はまだ `*unassigned*` のままである。
したがって (stream-map f y) が未代入 y を参照し、失敗する。

## 2. 本文の戦略で掃き出した場合

本文の戦略で掃き出すと以下のようになる

```scheme
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
```

こちらは set! y ... を先に行うため、(stream-map f y) を評価する時には y が代入済み。
また (delay dy) は遅延評価なので、その時点で dy が未代入でも直ちに参照されない。
したがって本文の戦略では solve は動作する。

## 結論

- 問題文の代替戦略: solve は動かない
- 本文の戦略: solve は動く
