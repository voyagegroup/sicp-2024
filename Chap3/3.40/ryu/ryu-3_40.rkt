#lang sicp

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

#|
P1: (lambda () (set! x (* x x)))
P2: (lambda () (set! x (* x x x)))

100: (abc vwxyz d): 10*10
1,000: (vwxy abcd z): 10*10*10
10,000: (vw abcd xyz): 10*10*100
10,000: (a vwxyz bcd): 10*1000
100,000: (v abcd wxyz): 10*100*100
1,000,000: (abcd vwxyz): 100*100*100
1,000,000: (vwxyz abcd): 1000*1000

xのなり得る値
100
1,000
10,000
100,000
1,000,000
|#

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

#|
1,000,000 (abcd vwxyz)
1,000,000 (vwxyz abcd)
なので、1,000,000 だけになる
|#