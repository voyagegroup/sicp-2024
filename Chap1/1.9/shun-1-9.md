練習問題1.9:次の⼆つの⼿続きは、どちらも inc, dec という⼿続  きによって⼆つの正の整数を加算する⽅法を定義している。⼿続  き inc は引数を 1増やし、 dec は引数を 1減らす。 

```scheme
 (define (+ a b)  
  (if (= a 0) b (inc (+ (dec a) b))))  
 
 (define (+ a b)  
  (if (= a 0) b (+ (dec a) (inc b))))  
```
 置換モデルを使って、それぞれの⼿続きが (+ 4 5) を評価する際に⽣成するプロセスを図⽰せよ。
 これらのプロセスは反復だろうか、それとも再帰だろうか。 

inc, decの本体
```scheme
(define (inc x)
 (+ x 1))

(define (dec x)
 (- x 1))
```

```scheme
(define (+ a b)  
  (if (= a 0) b (inc (+ (dec a) b))))   

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5 ))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
=9
```
これは遅延演算による膨張と計算による収束を行う再帰的プロセスである。

途中式
```scheme
(define (+ a b)  
  (if (= a 0) b (inc (+ (dec a) b))))   

(+ 4 5)
(if (= a 0) b (inc (+ (dec a) b)))
(if (= 4 0) b (inc (+ (dec a) b))) ;ifは特殊形式なので条件節を最初に評価する
((inc (+ (dec a) b))) ;4と0は異なるのでfalse。代替節の評価を行う
(inc (+ (- 4 1) 5)) ;組み合わせの部分式の仮パラメータを実引数で置き換え、手続きを評価する
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5))) ;条件式の評価は省略
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc (if (= 0 0) b (inc (+ (dec a) b)))))))
(inc (inc (inc (inc (if true 5 (inc (+ (dec a) b)))))))
(inc (inc (inc (inc 5 ))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
=9
```


```scheme
(define (+ a b)  
  (if (= a 0) b (+ (dec a) (inc b)))) 

(+ 4 5)　
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
=9
```
これは状態変数を更新するのみで、膨張も収縮もせず、反復的である。

途中式
```scheme
(define (+ a b)  
  (if (= a 0) b (+ (dec a) (inc b)))) 

(+ 4 5)
(if (= a 0) b (+ (dec a) (inc b))))
(if (= 4 0) b (+ (dec a) (inc b)))) ;ifは特殊形式なので条件節を最初に評価する
(if false b (+ (dec a) (inc b)))) ;条件節がfalseなので代替節を評価
(+ (- 4 1) (inc 5))　;部分式を評価する(仮パラメータを対応する引数で置き換える)
(+ (- 4 1) (+ 5 1))　;部分式を評価する(仮パラメータを対応する引数で置き換える)
((+ 3 (+ 5 1)))　
(+ 3 6)　;部分式を評価する(仮パラメータを対応する引数で置き換える)
(if (= 3 0) 6 (+ (dec 3) (inc 6)))
(+ (dec 3) (inc 6)); 条件節がfalseなので代替節を評価
(+ 2 7); 条件節がfalseなので代替節を評価
(+ 1 8)
(+ 0 9)
(if (= 0 0) b (+ (dec a) (inc b)))
(if true 9 (+ (dec a) (inc b)))
=9
```