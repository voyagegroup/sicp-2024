```racket
#lang racket

(require amb)

(define (a-pythagorean-triple low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(stream->list
 (in-amb
  (a-pythagorean-triple 2)))
```

ambの評価器はある選択点で全ての道が失敗するまで探索を行う。選択が失敗したときには最も近い選択点に戻る。
`(a-pythagorean-triple 2)`とすると、(i,j,k) = (2,2,2)から初めて、そこから最も近い選択点であるkを増やして(2,2,3), (2,2,4)...と探索を行う。
しかし、k^2 = 8となる整数kは存在しないため、その探索は終わることがなく無限に探索がされてしまう。


```racket
#lang racket

(require amb)

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (require p)
  (if (not p) (amb) 'done))

(define (an-integer-between low high)
  (require (not (> low high)))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple low)
  (let ((high (an-integer-starting-from low)))
    (a-pythagorean-triple-between low high)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(for/list ((x (in-amb (a-pythagorean-triple 2)))
           (_ (in-range 10)))
  x)
; '((3 4 5) (3 4 5) (3 4 5) (3 4 5) (3 4 5) (3 4 5) (6 8 10) (3 4 5) (6 8 10) (3 4 5))
```

内部でhighに1ずつ追加し、betweenを呼び出すという形に変更した。
これでhighが終了条件になり、無限に探索される問題を解消できる。
