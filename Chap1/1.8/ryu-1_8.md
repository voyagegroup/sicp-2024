#et  問1.8

### とりあえず、手で計算してみる

x=8, y0=1
→ 3.33

x=8, y1=3.33
→ 2.4606

x=8, y2=2.4606
→ 2.081

ちゃんと2に近づいていっている

### 解いてみる

```
(define (good-enough-cube? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (square a) (* a a))


(define (newton guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(newton 1 8)
; → 3 1/3

(define (cube-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-iter (newton guess x) x)))

(define (cube x)
  (cube-iter 1.0 x))
(cube 8)
; → 2.000004911675504
(cube 10)
; → 2.1544959251533746
```

### 手続きを追っていく

```
(cube 8)
; 手続きの本体を取り出す
(cube-iter 1.0 8)
; 手続きの本体を取り出す
(if (good-enough-cube? 1.0 9)
    1.0
    (cube-iter (newton 1.0 8) 8))

; predicateを評価
(good-enough-cube? 1.0 9)
(< (abs (- (* 1.0 1.0 1.0) 8)) 0.001)
(< (abs (- 1.0 8)) 0.001)
(< (abs 7) 0.001)
(< 7 0.001)
; -> #f
; #fなので、alternativeを評価
(cube-iter (newton 1.0 8) 8)
; 部分式を評価
(newton 1.0 8)
; 手続きの本体を取り出す
(/ (+ (/ 8 (square 1.0)) (* 2 1.0)) 3)
; -> 3.3333
(cube-iter 3.3333 8)
; 手続きの本体を取り出す
(if (good-enough-cube? 3.3333 8)
    3.3333
    (cube-iter (newton 3.3333 8) 8))
```
