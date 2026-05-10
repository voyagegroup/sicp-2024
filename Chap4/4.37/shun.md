## 4.35

```racket
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(a-pythagorean-triple-between 2 5)
```

この版では、(2,2,2), (2,2,3),(2,2,4)...と探索する。

## 4.37

```racket

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

(a-pythagorean-triple-between 2 5)
```

この版では(2,2,√8), (2, 3, √13), (2, 4, √20)...と、kがiとjから決まり、i,jの探索のみを行うだけで済むため効率が良い。