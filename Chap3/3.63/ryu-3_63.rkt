#lang sicp

; 本文中

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(display-stream (sqrt-stream 2))
; 

; Louisバージョン

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

; Louisのバージョンは、新しいcons-streamが毎回作られることになる。
; 指数的に増えていくことになる。

; lamdbaの場合
; 同様に、呼び出しの旅に再計算が走るので、冗長な計算をする。
; Louisのバージョンとlambdaのバージョンとを比較すると、Louisのバージョンのほうが、streamを毎回作るコストの分が非効率になる。