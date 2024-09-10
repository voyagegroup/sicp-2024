## 問題1.3

```
(define (square a) (* a a))

(square 3)
; 9


(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 1 2)
; 5

(define (sum-of-suquare-two a b c)
 (cond ((and (>= b a) (>= c a)) (sum-of-squares b c))
       ((and (>= c b) (>= a b)) (sum-of-squares a c))
       ((and (>= a c) (>= b c)) (sum-of-squares a b))))


(sum-of-suquare-two 4 2 3)
; 25
```

