# 問題 4.37

Ben の主張（「問題 4.35 よりずっと効率がよい」）は**正しい**。

## 問題 4.35 の探索空間

```scheme
(let ((i (an-integer-between low high)))    ; n 通り
  (let ((j (an-integer-between i high)))    ; 最大 n 通り
    (let ((k (an-integer-between j high)))  ; 最大 n 通り
      (require (= (+ (* i i) (* j j)) (* k k)))
      (list i j k))))
```

`i`, `j`, `k` をそれぞれ amb で列挙するため、調べる可能性の数は **O(n³)**。

## Ben の手続きの探索空間

```scheme
(let ((i (an-integer-between low high))
      (hsq (* high high)))
  (let ((j (an-integer-between i high)))    ; 最大 n 通り
    (let ((ksq (+ (* i i) (* j j))))
      (require (>= hsq ksq))                ; k > high になる対を枝刈り
      (let ((k (sqrt ksq)))                 ; k を amb で列挙せず直接計算
        (require (integer? k))
        (list i j k)))))
```

`k` を amb で列挙せず `i²+j²` の平方根として直接求める。
調べる可能性は `(i, j)` の対だけなので **O(n²)**。
さらに `(require (>= hsq ksq))` で k が high を超える対を早期に枝刈りする。
