次の手続きはAckermann関数という数学関数を計算する.
```scheme
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
```

次の式の値は何か.
```scheme
(A 1 10)
(cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(cond (false 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(cond (false 0)
        (false (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(cond (false 0)
        (false (* 2 y))
        (false 2)
        (else (A (- x 1)
                 (A x (- y 1)))))) ; 合成手続きの部分式を評価する(仮パラメータを対応する引数で置き換える)
(cond (false 0)
        (false (* 2 y))
        (false 2)
        (else (A (- 1 1)
                 (A 1 (- 10 1))))))
(cond (false 0)
        (false (* 2 y))
        (false 2)
        (else (A (- 1 1)
                 (A x (- 10 1))))))
(cond (false 0)
        (false (* 2 y))
        (false 2)
        (else (A (- 1 1)
                 (A 1 9)))))
(cond (false 0)
        (false (* 2 y))
        (false 2)
        (else (A 0
                 (A 1 9))))); 合成手続きの部分式を評価する(仮パラメータを対応する引数で置き換える
(cond ...;  falseの条件式を省略
        (else (A 0
                 (cond ((= 9 0) 0)
                    ((= 1 0) (* 1 y))
                    ((= 9 1) 1)
                    (else (A (- 1 1)
                             (A 1 (- 9 1))))))
(cond ...
        (else (A 0
                 (cond ((= 9 0) 0)
                    ((= 1 0) (* 1 y))
                    ((= 9 1) 1)
                    (else (A 0
                             (A 1 8)))))
(cond ...
        (else (A 0
                 (cond ((= 9 0) 0)
                    ((= 1 0) (* 1 y))
                    ((= 9 1) 1)
                    (else (A 0
                             (A 1 1))))); yが1になるまでループ
```
=1024


(A 2 4)
=65536

(A 3 3)
=65536
```
Aを上で定義した手続きとして, 次の手続きを考える.
```scheme
(define (f n) (A 0 n))
2n

(define (g n) (A 1 n))
nが1なら2。nが2なら4。nが3なら8..
2のn乗

(define (h n) (A 2 n))
nが1なら2,2なら4(2の2乗),3なら16(2の4乗),4なら65536(2の16乗)
2^2^(A 2 (- n 1))


(define (k n) (* 5 n n))
```
正の整数nに対して手続きf, gおよびhが計算する関数の簡潔な数学的定義を述べよ. 例えば(k n)は5n^2を計算する.