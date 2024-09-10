## 1.6

### もともとのsqrt-iter

```
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))


(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square a) (* a a))

(define (sqrt x)
  (sqrt-iter 1.0 x))


(sqrt 9)
```

### new-ifをつかったsqrt-iter

```
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
; → 5
(new-if (= 1 1) 0 5)
; → 0

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(new-sqrt 2)
```

### 答え

new-ifが無限ループしてしまう。

```
(sqrt-iter 3.0001 9)
```

このとき、もともとのsqrt-iterの場合、

```
(if (good-enough? 3.0001 9)
  3.0001
  (sqrt-iter (improve 3.0001 9) 9))
```

```
(good-enough? 3.0001 9)
→ true
```

```
(if (true)
  3.0001
  (sqrt-iter (improve 3.0001 9) 9))
```
となる。

ifの定義は「if式を評価するには, 解釈系は式の 述語⟨predicate⟩部分を評価する. ⟨predicate⟩の評価の結果が真なら, 解釈系は 帰結部⟨consequent⟩を評価しその値を返す. そうでなければ, 代替部⟨alternative⟩を評価し, その値を返す.」なので、3.0001が結果として返される。

new-ifを用いた場合、

```
(new-if (good-enough? 3.0001 9)
  3.0001
  (sqrt-iter (improve 3.0001 9) 9))
```

```
(cond ((good-enough? 3.0001 9) 3.0001)
  (else (sqrt-iter (improve 3.0001 9))))
```

正規順序は、「 基本的演算子だけを持つ式が出てくるまで, パラメタへの被演算子の式の置換えを繰り返し, それから評価を行う.」。
そのため、`(good-enough? 3.0001 9)` がtrueになるので、そのまま処理を打ち切って、`3.0001`を出力してほしいのだが、そのまま、elseを処理し始めてしまう。

```
(sqrt-iter (improve 3.0001 9))
```

結果として、condがtrueになるはずなのに、延々とelseが処理され続けてしまう。

