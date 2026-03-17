#lang sicp


(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

#|
## Ben

x = 10
b = 1 + 10 = 11
a = 5
(+ a b) = 16

## Alyssa

(let ((b '*unassigned*)
      (a '*unassigned*))

b = (+ a x)
aはまだunassignedなのでエラーになる

## Eva

b = (+ 5 10)
a = 5
(+ a b) = 20

## Ben考察

Benは当初の修正前の状態？
本文中の例の(odd?) と (even?) が正しく評価できないんじゃない？
相互にやっている系の。

## Eva考察

Evaの内部定義を実装する方法は、同じ環境内での依存関係を紐解いて、単純なものから処理（今回の場合は、(= a 5)）から処理をしていく。

Evaは循環参照おこると壊れそう。

(define a (+ b 1))
(define b (+ a 1))

みたいなのを同じ環境でやった瞬間破綻する


|#