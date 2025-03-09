#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;1 1/2

(fold-left / 1 (list 1 2 3))
;1/6

(fold-right list '() (list 1 2 3))
;'(1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
;'(((() 1) 2) 3)

; q: fold-rightとfold-leftが, どのような並びに対しても同じ値を生じるためにopが満たすべき性質は何か.
; a: (list 1 2 3)があったとして、(op 1 (op 2 3)) = (op (op 1 2) 3) が成り立つ必要がある。
; fold-rightではopの第2引数から再帰的に評価されて、listの末尾から計算されるが、fold-leftではlistの1番目から計算されるため。
