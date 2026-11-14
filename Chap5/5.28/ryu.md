5.4.2節に述べたようにev-sequenceを変更して評価器の定義を修正し, 評価器が最早末尾再帰的でないようにせよ. 問題5.26と5.27を再び走らせ, 両方の版のfactorial手続きが今回は入力に線形に成長するスペースを必要とすることを示せ.


```
Welcome to DrRacket, version 8.12 [cs].
Language: sicp, with debugging; memory limit: 128 MB.


;;; EC-Eval input:
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 1)

(total-pushes = 70 maximum-depth = 17)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 107 maximum-depth = 20)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 144 maximum-depth = 23)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4
)
(total-pushes = 181 maximum-depth = 26)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 218 maximum-depth = 29)
;;; EC-Eval value:
120

;;; EC-Eval input:
.
```

```
Welcome to DrRacket, version 8.12 [cs].
Language: sicp, with debugging; memory limit: 128 MB.


;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:

(total-pushes = 18 maximum-depth = 11)
;;; EC-Eval value:
1

;;; EC-Eval input:

(total-pushes = 52 maximum-depth = 19)
;;; EC-Eval value:
2

;;; EC-Eval input:

(total-pushes = 86 maximum-depth = 27)
;;; EC-Eval value:
6

;;; EC-Eval input:

(total-pushes = 120 maximum-depth = 35)
;;; EC-Eval value:
24

;;; EC-Eval input:

(total-pushes = 154 maximum-depth = 43)
;;; EC-Eval value:
120

;;; EC-Eval input:
.
```

実験まとめ

| 項目 | 5.26（反復的） | 5.27（再帰的） | 5.28（反復的） | 5.28（再帰的） |
|---|---|---|---|---|
| total-pushes | 35n + 29 | 32n - 16 | 37n + 33 | 34n - 16 |
| maximum-depth | 10 | 5n + 3 | 3n + 14 | 8n + 3 |

`ev-sequence` を変更し、最後の式の評価でも `unev` と `env` を退避して、評価後に戻るようにした。

5.26の反復はdepthが10だったが、5.28の反復では一定ではにことがわかった。

変更後は、末尾呼び出しでも呼び出し元へ戻るための情報がスタックに残るため膨張するようになってしまった。



