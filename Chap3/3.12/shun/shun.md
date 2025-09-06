```racket
#lang sicp

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)


(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

z
; (a b c d)

(cdr x)
; (b)

(define w (append! x y))

w
; (a b c d)

(cdr x)
; (b c d)
```

## ~ 最初の(cdr x)まで

![alt text](image.png)

## (append! x y)の後

xのlast-pair ('b nil)のcdr(つまりnil)に対してy('c 'd)をsetするため、以下のような形になる。

![alt text](image-1.png)