#lang sicp


(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))


#|
P1: (s (lambda () (set! x (* x x))))
P2: (s (lambda () (set! x (+ x 1))))

(abc xyz) → 101
(xyz abc) → 121

o: 101:	    	P1がxを100に設定し, 次にP2がxを101に増加する.
o: 121:		P2がxを11に増加し, 次にP1がxをx掛けるxに設定する.
x: 110:		P1が(* x x)の評価でxの値に二度アクセスする間にP2がxを10から11に変える.
x: 11:		P2がxにアクセスし, P1がxを100に設定, P2がxを設定する.
x: 100:		P1がxに(二回)アクセスし, P2がxを11に設定し, P1がxを設定する.
|#

(parallel-execute (lambda ()(set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

#|
P1: (lambda ()(set! x ((s (lambda () (* x x))))))
P2: (s (lambda () (set! x (+ x 1))))

(abc xyz) → 101
(xyz abc) → 121
(ab xyz c) → 100

o: 101:	    	P1がxを100に設定し, 次にP2がxを101に増加する.
o: 121:		P2がxを11に増加し, 次にP1がxをx掛けるxに設定する.
x: 110:		P1が(* x x)の評価でxの値に二度アクセスする間にP2がxを10から11に変える.
x: 11:		P2がxにアクセスし, P1がxを100に設定, P2がxを設定する.
o: 100:		P1がxに(二回)アクセスし, P2がxを11に設定し, P1がxを設定する.
|#