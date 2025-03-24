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

;; ここから解答

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

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens) ;; k いる？
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

(queens 5)

;; MxNの盤面にも自然に拡張できそう
