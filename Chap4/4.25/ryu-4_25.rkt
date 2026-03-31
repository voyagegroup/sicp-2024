#lang sicp

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)

; 1
(unless (= 5 1)
  (* 5 (factorial (- 5 1)))
  1)

; 2
(if (= 5 1) ; #f
    1
    (* 5 (factorial 4))) ; こっち

; 3
(* 5 (factorial 4))
(* 5
   (unless (= 4 1)
     (* 4 (factorial 3))
     1))

; 4
(* 5
   (if (= 4 1) ; #f
       1
       (* 4 (factorial 3)))) ; こっち

; 中略

; 5
(* 5 (*4 (*3 (*2
              (unless (= 1 1)
                (* 1 (factorial 0)
                   1))))))

; 6-a
(* 5 (*4 (*3 (*2
              (if (= 1 1)
                  1
                  (* 1 (factorial 0)))))))


; 6-b
(* 5 (*4 (*3 (*2
              (unless (= 1 1)

                
                ; (factorial 0)
                (* 1
                   (unless (= 0 1)
                     (* 0 (factorial (- 0 1)))
                     1)) 
                
                1)))))


#|
Q. (factorial 5)を評価しようとすると何が起きるか.

6-aのように、ifに展開され、ifの帰結部を返して、処理を終わらしてほしい。

>  Schemeは作用的順序(applicative order)の言語であるといった. つまりScheme手続きへの引数は, 手続きを作用する時にすべて評価する.

であるため、6-bのように、引数の (factorial 0) を評価し、さらに、(factorial -1), (factorial -2) と、先のfactorialも無限に評価しようとする。

Q. われわれの定義は正規順序の言語では動くだろうか.
動かず、無限ループする
|#
                

