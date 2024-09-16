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
          (new-sqrt-iter (improve guess x) x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(new-sqrt 2)
```

### 答え

new-ifが無限ループしてしまう。

### 処理を追っていく

もともとのsqrt-iterとnew-ifを使ったnew-sqrt-iterをつかって`(sqrt 9)` の処理を追ってみる。

√9は3なので、答えは3が出力されるのを想定。

ニュートン法だと、各ステップでは以下の結果になるはず。

| 既定値 | 商 | 平均 |
| --- | --- | --- |
| 1 | (9/1) = 9 | ((9 + 1) / 2) = 5 |
| 5 | (9/5) = 1.8 | ((1.8 + 5) / 2) = 3.4 |
| 3.4 | (9/3.4) = 2.65 | ((2.65 + 3.4) / 2) = 3.025 |

#### sqrt-iterの処理を追ってみる

手続きを追ってみる

```lisp
(sqrt 9)
; sqrtの本体を取り出し、仮パラメタを対応する引数で取り替え、手続きの本体を評価する
(sqrt-iter 1.0 9)
; sqrt-terの本体を取り出し、仮パラメタを対応する引数で取り替え、手続きの本体を評価する
(if (good-enough? 1.0 9)
    1.0
    (sqrt-iter (improve 1.0 9) 9))
; ifの定義: 「if式を評価するには, 解釈系は式の 述語⟨predicate⟩部分を評価する. ⟨predicate⟩の評価の結果が真なら, 解釈系は 帰結部⟨consequent⟩を評価しその値を返す. そうでなければ, 代替部⟨alternative⟩を評価し, その値を返す.」
; predicateのgood-enough?を評価する
(good-enough? 1.0 9)
; good-enough?の本体を取り出し、仮パラメタを対応する引数で置き換え、手続きの本体を評価する
(< (ads (- (square 1.0) 9)) 0.001)
; 部分式を評価
(ads (- (square 1.0) 9))
; -> squareの手続きの本体を評価
(ads (- (* 1.0 1.0) 9))
; 演算子を引数に作用させる
(ads (- 1 9))
(ads 8)
; -> 8
(< 8 0.001)
; -> #f
; ifの定義: 「if式を評価するには, 解釈系は式の 述語⟨predicate⟩部分を評価する. ⟨predicate⟩の評価の結果が真なら, 解釈系は 帰結部⟨consequent⟩を評価しその値を返す. そうでなければ, 代替部⟨alternative⟩を評価し, その値を返す.」
; predicateのgood-enough?が#fなので、alternativeを評価し、その値を返す
(sqrt-iter (improve 1.0 9) 9)
; 部分式のimproveを評価
(improve 1.0 9)
; improveの本体を取り出し、仮パラメタを対応する引数で置き換え、手続きの本体を評価する
(average 1.0 (/ 9 1.0))
; 部分式を評価
; 演算子を引数に作用
(average 1.0 9)
; averageの本体を取り出し、仮パラメタを対応する引数で取り替え、手続きの本体を評価する
(/ (+ 1.0 9) 2)
; 部分式を評価
; 演算子を引数に作用
(/ 10.0 2)
; -> 5.0
(sqrt-iter 5.0 9)
; sqrt-terの本体を取り出し、仮パラメタを対応する引数で取り替え、手続きの本体を評価する
(if (good-enough? 5.0 9)
    5.0
    (sqrt-iter (improve 5.0 9) 9)
; 以下、good-enough?が#tになるまで、再帰的に呼ばれ続ける
; good-enough?が#tになるとconsequentを評価し値を返して終わる
```

#### new-sqrt-iteの処理を追ってみる

```
(new-sqrt 9)
; new-sqrtの本体を取り出して、仮パラメタの置き換えと本体の評価
(new-sqrt-iter 1.0 9)
; new-sqrt-iterの本体を取り出して、仮パラメタの置き換えと本体の評価
(new-if (good-enough? 1.0 9)
        1.0
        (new-sqrt-iter (improve 1.0 9) 9))
; 部分式を評価
; (good-enough? 1.0 9) -> #f だったのでこっちは省略
; 部分式のnew-sqrt-iterの本体を取り出して、仮パラメタの置き換えと本体の評価
(new-sqrt-iter (improve 1.0 9) 9)
; 部分式のimproveを評価
; (improve 1.0 9) -> 5.0 だったので省略
(new-sqrt-iter 5.0 9)
(new-if (good-enough? 5.0 9)
        5.0
        (new-sqrt-iter (improve 5.0 9) 9))
; となっていく。
; ...中略
; good-enough?が#tになるであろう、guess = 3.0001をみる
(new-if (good-enough? 3.0001 9)
        3.0001
        (new-sqrt-iter (improve 3.0001 9)))
; good-enough? -> #tになるので、3.0001が出力されてほしいのだが、部分式のnew-sqrt-iterの評価に移行してしまう。
; 結果としてループする
```


### 手続きを追っていないときの答え

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

