#lang racket

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 206 40)
; 正規順序評価
;gcd 206 40
; if (= 40 0) => false
; gcd 40 (remainder 206 40) -> 1
; if (= (remainder 206 40) 0)
; if (= 6 0) => false
; gcd (remainder 206 40) (remainder 40 (remainder 206 40))
; if (= (remainder 40 (remainder 206 40)) 0) -> 2
; if (= 4 0) => false
; gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) -> 4
; if (= 2 0) => false
; gcd ((remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder(40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; if (= (remainder ((remainder(40 (remainder 206 40))) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) 0) => true -> 7
; (remainder ((remainder 206 40) (remainder(40 (remainder 206 40))))) -> 4
; remainderは18回

;
;gcd 206 40
;if (= 40 0)=>false
;gcd 40 (remainder 206 40)) -> 1
;gcd 40 6
;if (= 6 0)=>false
;gcd 6 (remainder 40 6) -> 1
;gcd(6 4)
;if (= 4 0)=>false
;gcd(4 (remainder 6 4)) -> 1
;gcd 4 2
;if (= 2 0)=>false
;gcd 2 (remainder 4 2) -> 1
;gcd 2 0
;if(= 0 0)
;2
; remainderは4回