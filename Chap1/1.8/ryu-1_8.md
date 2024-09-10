## 問1.8

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

