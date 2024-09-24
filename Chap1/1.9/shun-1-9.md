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
(if (= a 0) b (inc (+ (dec a) b)))
(if (= 4 0) b (inc (+ (dec a) b))) ;ifは特殊形式なので条件節を最初に評価する
(if false b (inc (+ (dec a) b))) ;4と0は異なるのでfalse。代替節の評価を行う
(if false b (inc (+ (- 4 1) 5))) ;組み合わせの部分式の仮パラメータを実引数で置き換え、手続きを評価する
(if false b (inc (+ 3 5))) ;部分式の仮パラメータを実引数で置き換え、手続きを評価する
(if false b (inc (+ 3 5))) 
(if false b (inc 8)) ;部分式の仮パラメータを対応する引数で置き換え、手続きを評価する
(if false b (+ 8 1))
(if false b 9)
=9
```

```scheme
(define (+ a b)  
  (if (= a 0) b (+ (dec a) (inc b)))) 

(+ 4 5)
(if (= a 0) b (+ (dec a) (inc b))))
(if (= 4 0) b (+ (dec a) (inc b)))) ;ifは特殊形式なので条件節を最初に評価する
(if false b (+ (dec a) (inc b)))) ;条件節がfalseなので代替節を評価
(if false b (+ (- 4 1) (inc 5))))　;部分式を評価する(仮パラメータを対応する引数で置き換える)
(if false b (+ (- 4 1) (+ 5 1))))　;部分式を評価する(仮パラメータを対応する引数で置き換える)
(if false b (+ 3 (+ 5 1))))　
(if false b (+ 3 6))))　
(if false b 9)))　
=9

```