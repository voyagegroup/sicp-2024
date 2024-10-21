#lang sicp
; 手続きが生成するプロセスはもちろん解釈系が使う規則に依存する.
; 例えば上で述べた反復的gcd手続きを考え, 1.1.5節で論じた正規順序評価を使って解釈したとする.
; (ifの正規順序評価規則は問題1.5に書いてある.)
; (正規順序の)置換え法を使い, (gcd 206 40)の評価で生成されるプロセスを図示し, 実際に実行されるremainder演算を指示せよ.
; (gcd 206 40)の正規順序評価で, remainder演算は実際に何回実行されるか.
; 作用的順序ではどうか.


; - 作用的順序
;	- 引数を評価し, 作用させる
; - 正規順序
;	- 完全に展開し, 簡約する



(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;(if (= b 0)
;    a
;    (gcd b (remainder a b)))

; 正規順序
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
; (= 40 0) -> #f
(gcd 40 (remainder 206 40))
(if(= (remainder 206 40) 0)
   40
   (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
; (remainder 206 40) -> 6
; remainderを1回実行
; (= 40 6) -> #f
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; a=(remainder 206 40) b=(remainder 40 (remainder 206 40))
(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; 
(remainder 40 (remainder 206 40))
; (remainder 40 (remainder 206 40)) -> (remainder 40 6) -> 4
; remainderを2回実行
; (= 4 0) -> #f
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; a=(remainder 40 (remainder 206 40))
; b=(remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) -> (remainder 6 (remainder 40 (remainder 206 40))) ->  (remainder 6 (remainder 40 6)) -> (remainder 6 4) ->2
; remainderを4回実行
; (= 0 2) -> #f
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; a=(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; b=(remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
(remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) -> (remainder (remainder 40 6) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) -> (remainder 4 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; -> (remainder 4 (remainder (remainder 206 40) (remainder 40 6)))) -> (remainder 4 (remainder (remainder 206 40) 4))) ->  (remainder 4 (remainder 6 4))) -> (remainder 4 2))  -> 0
; remainderを7回実行
; (= 0 0) -> #t

; 帰結部⟨consequent⟩を評価しその値を返す
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) -> (remainder (remainder 206 40) (remainder 40 6)) -> (remainder 6 (remainder 40 6)) -> (remainder 6 4) -> 2
; remainderを4回実行

; --------
; 実行されたremanderを足し合わせる
; 1 + 2 + 4 + 7 + 4 -> 18
; 正規順序では18回実行された
; --------


; 作用的順序
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
; (= 40 0) -> #f
(gcd 40 (remainder 206 40))
; (remainder 206 40) -> 6
; remainderを1回実行
(gcd 40 6)
(if (= 6 0)
    40
    (gcd 6 (remainder 40 6)))
; (= 6 0) -> #f
(gcd 6 (remainder 40 6))
; (remainder 40 6) -> 4
; remainderを1回実行
(gcd 6 4)
(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))
; (= 4 0) -> #f
(gcd 4 (remainder 6 4))
; (remainder 6 4) -> 2
; remainderを1回実行
(gcd 4 2)
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))
; (= 2 0) -> #f
(gcd 2 (remainder 4 2))
; (remainder 4 2) -> 0
; remainder を1回実施
(gcd 2 0)
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))
; (= 0 0) -> #t
; 帰結部を返す
; -> 2

; --------
; 実行されたremanderを足し合わせる
; 1 + 1 + 1 + 1 -> 4
; 作用的順序では4回実行された
; --------














