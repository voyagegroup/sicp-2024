## a. 実験まとめ

| 説明 | 式 | total-pushes 前→後 | maximum-depth 前→後 |
|---|---|---:|---:|
| 演算子が記号 → 新しい経路 | `(f 'x 'y)` | 16 → 12 | 5 → 5 |
| 演算子が式 → 外側は従来の経路。内側の `get-f` と本体の `list` は新しい経路 | `((get-f) 'x 'y)` | 19 → 15 | 6 → 5 |
| 演算子が記号 → 新しい経路。被演算子内の `g`、両手続きの本体の `list` も対象 | `(f (g 'x) y)` | 29 → 21 | 10 → 10 |
| 上と同じ。最後の引数が引用式でも退避回数は同じ | `(f (g 'x) 'y)` | 29 → 21 | 10 → 10 |
| 演算子が記号・引数なし → 新しい経路。`continue` の退避だけ残る | `(zero)` | 3 → 1 | 3 → 1 |
| 外側の `cons` と内側の `zero` がともに新しい経路 | `(cons (zero) '())` | 11 → 7 | 8 → 6 |

## 修正前

```
;; 1. 演算子が記号 → 新しい経路
;;; EC-Eval input:
(define (f x y)
  (list x y))

(f 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 16 maximum-depth = 5)
;;; EC-Eval value:
(x y)


;; 2. 演算子が式 → 外側は従来の経路

;;; EC-Eval input:
(define (get-f)
  f)

((get-f) 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 19 maximum-depth = 6)
;;; EC-Eval value:
(x y)


;; 3. 被演算子の中で手続きを呼ぶ

;;; EC-Eval input:
(define (g x)
  (list 'g x))

(define y 'Y)

(f (g 'x) y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 29 maximum-depth = 10)
;;; EC-Eval value:
((g x) Y)

;;; EC-Eval input:
(f (g 'x) 'y)


(total-pushes = 29 maximum-depth = 10)
;;; EC-Eval value:
((g x) y)


;; 4. 引数なしの呼び出し
;;; EC-Eval input:
(define (zero)
  42)

(zero)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
42

;; 5. 呼び出し後に計算を続ける

;;; EC-Eval input:
(cons (zero) '())


(total-pushes = 11 maximum-depth = 8)
;;; EC-Eval value:
(42)
```

## 修正後

```
;; 1. 演算子が記号 → 新しい経路


;;; EC-Eval input:
(define (f x y)
  (list x y))

(f 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 12 maximum-depth = 5)
;;; EC-Eval value:
(x y)


;; 2. 演算子が式 → 外側は従来の経路
;;; EC-Eval input:
(define (get-f)
  f)

((get-f) 'x 'y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 15 maximum-depth = 5)
;;; EC-Eval value:
(x y)


;; 3. 被演算子の中で手続きを呼ぶ
;;; EC-Eval input:
(define (g x)
  (list 'g x))

(define y 'Y)

(f (g 'x) y)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 21 maximum-depth = 10)
;;; EC-Eval value:
((g x) Y)

;;; EC-Eval input:
(f (g 'x) 'y)


(total-pushes = 21 maximum-depth = 10)
;;; EC-Eval value:
((g x) y)


;; 4. 引数なしの呼び出し
;;; EC-Eval input:
(define (zero)
  42)

(zero)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 1 maximum-depth = 1)
;;; EC-Eval value:
42


;; 5. 呼び出し後に計算を続ける
;;; EC-Eval input:
(cons (zero) '())


(total-pushes = 7 maximum-depth = 6)
;;; EC-Eval value:
(42)

```

## b

評価器の処理が複雑になりすぎる気配がある。
そうなると保守も、デバッグもしんどくなると思う。

低いレイヤーはシンプルな状態にして、上のレイヤーに複雜さをもったほうが使いやすいと思う。
なので、複雑な最適化は、翻訳時に済ませて、評価器はじっこうするだけのほうが良いと思う。


