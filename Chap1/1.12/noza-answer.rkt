#lang racket

; x行y列目の要素を取得すると考える
; x < y の場合は、値がおかしいことを示す
; x = y の場合、1
; y = 1 の場合、1
; それ以外の場合、(x-1)行(y-1)列目の要素と(x-1)行y列目の要素の和

(define (pascals-triangle x y)
  (cond
    ((< x y) (display "x must be less than or equal to y"))
    ((= x y) 1)
    ((= y 1) 1)
    (else (+ (pascals-triangle (- x 1) (- y 1))
             (pascals-triangle (- x 1) y)))))

(pascals-triangle 4 2) ; => 3
(pascals-triangle 5 3) ; => 6
(pascals-triangle 5 5) ; => 1

; これはフィボナッチ数列の時と同様に、関数を取り出すときに枝が２本の木構造を作成しながら膨張する
; 実際の計算は関数の取り出しと評価が終わった後に行われるため、再帰的プロセスとなる
