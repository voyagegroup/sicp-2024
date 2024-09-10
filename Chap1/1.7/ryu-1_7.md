## 問1.7


### 問題に関する関数
```
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square a) (* a a))
```

### 動作を思い出すメモ

```
(good-enough? 1.0 9)
(< (abs (- (square 1.0) 9)) 0.001)
(< (abs (- (* 1.0 1.0) 9)) 0.001)
(< (abs (- 1.0 9)) 0.001)
(< (abs 8) 0.001)
(< 8 0.001)
→ false
```

```
(good-enough? 2.5 9)
(< (abs (- (square 2.5) 9)) 0.001)
(< (abs (- 6.25 9)) 0.001)
(< 2.75 0.001)
→ false
```

```
(good-enough? 3.0 9)
(< (abs (- (square 3.0) 9)) 0.001)
(< (abs (- 9 9)) 0.001)
(< 0 0.001)
→ true
```

### 小さい値

元が小さいと、差分のしきい値の0.001をすぐに下回ってしまうため、十分な制度が出る前にgood-enough?がtrueになってしまう。

### 大きい値

元が大きいと、good-enough?がtrueになるまで相当数処理をし続けてしまう。

### 差分を使った推定

> good-enough?を実装するもう一つの戦略は, ある繰返しから次へのguessの変化に注目し, 変化が予測値に比べ非常に小さくなった時に止めるのである. こういう終了テストを使う平方根手続きを設計せよ.

もともとのgood-enough?は、guessを二乗した値とyとを比較して、十分かを評価している。

次へのguessの変化に注目し, 変化が予測値に比べ非常に小さくなった時に止める
ということは、前後のguessを使うってことかな？
(good-enough-v2? guess prev-guess)

```
(define (good-enough-v2? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.001))

(define (sqrt-iter-v2 guess prev-guess x)
  (if (good-enough-v2? guess prev-guess)
      guess
      (sqrt-iter-v2 (improve guess x) guess x)))


(define (sqrt-v2 x)
  (sqrt-iter-v2 1.0 0.0 x))
(sqrt-v2 0.002)
; → 0.04472230608683239
```

それっぽくならない




