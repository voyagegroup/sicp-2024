## (define x (cons 1 2))でxが束縛されるとき
![alt text](image.png)

## (define z (cons x x))

![alt text](image-1.png)

## (set-car! (cdr z) 17)
![alt text](image-2.png)

## (car x)
![alt text](image-3.png)

```racket
#lang sicp

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)


(define (car z) (z 'car))


(define (cdr z) (z 'cdr))


(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)


(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(car x)
```