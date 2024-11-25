#lang racket

(define (square x)
  (* x x)) 

(define (f g)
  (g 2))

(f square)

(f f)
#| (f 2) |#
#| (2 2) |#
#| となり2は演算子ではないのでエラーになって終了する。 |#
