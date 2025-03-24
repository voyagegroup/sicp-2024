#lang racket

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens) ;; k いる？
  (display new-row)
  (display " ")
  (display rest-of-queens)
  (newline)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (define (safe-row? x n)
    (if (= n k)
      #t
      (let ((y (list-ref positions n))) ;; list-ref はリストのn番目の要素を返す
          (and (not (= x y))
               (not (= (abs (- x y)) n))
               (safe-row? x (+ n 1))))))
  (safe-row? (car positions) 1))


(define (queens board-size)
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

(define (queens-2 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
           (map (lambda (rest-of-queens)
                 (adjoin-position new-row k rest-of-queens))
                (queen-cols (- k 1))))
           (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(display "queens")
(newline)
(queens 4)
(newline)
(display "queens-2")
(newline)
(queens-2 6)

;; 明らかにqueens-2の方がadjoin-positionが多く呼び出されている
;; queens の場合は、ざっくり 1*k + 2*k + 3*k + ... + k*k 回 処理が行われる
;; queens-2 の場合は、queen-cols がネストしているので、(k^1)k + (k^2)k + ... + (k^k)k 回 処理が行われる
;; よって、queens-2はqueensのk^k倍の処理時間がかかる
