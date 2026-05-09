#lang racket

(require amb)

(define (require p)
  (unless p (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; an-integer-between を an-integer-starting-from に単純置換すると:
;   i=1, j=1 固定のまま k が 1,2,3,... と無限に試され続け
;   深さ優先探索が無限枝に捕まって j=2 や i=2 に到達できない。
;
; 解決策: k（最大値）を無限列の外側に置き、i と j を k で上から抑える。
;   各 k に対して (i, j) の組合せは有限なので探索が必ず終わり、
;   次の k に進める。これにより全てのピタゴラス三角形に到達できる。
(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

; 最初の5解を表示
(define s (in-amb (a-pythagorean-triple)))
(for ([triple (stream-take s 5)])
  (displayln triple))
