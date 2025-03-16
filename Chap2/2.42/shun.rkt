#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (safe? k positions) 
 (define (safe-itr x n)
   (or (= n k)
       (let ((y (list-ref positions n)))
            (and (not (= x y)) ; 上の行にいないか
                 (not (= (abs (- x y)) n)) ; xとyの絶対値は
                 (safe-itr x (+ n 1))))))
 (safe-itr (car positions) 1))

; ex positions: (safe? 2 (3 5 1))
; (safe-itr 2 1) x=3 n=1 y=5
; (safe-itr 2 2) x=3 n=2 y=1 (abs (- 3 1))=2 = n にnotがついてfalse
; xは最新の追加されたクイーンの列番号
; nはpositionsのインデックス、つまり0から始まる行番号
; yはpositionsのn番目の要素、つまりn行目のクイーンの列番号
; 斜めにいる場合、列の差と行の差が等しい
; つまり、abs(x-y) = n が成り立つとき、斜めにいる

(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens); なぜkを引数にとったのかはわからなかった
    (cons new-row rest-of-queens))
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 5)