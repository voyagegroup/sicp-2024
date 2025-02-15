#lang racket

(define (square n)
  (* n n))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items null))

; 仮パラメタを実引数で置き換える
(define (square-list (list 1 2 3 4))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter (list 1 2 3 4) null))

; 仮パラメタを実引数で置き換える
(define (iter (list 1 2 3 4) null)
    (if (null? (list 1 2 3 4))
        null
        (iter (cdr (list 1 2 3 4)) 
              (cons (square (car (list 1 2 3 4)))
                    null))))

(define (iter (list 1 2 3 4) null)
        (iter (cdr (list 1 2 3 4)) 
              (cons (square 1)
                    null)))

(define (iter (list 1 2 3 4) null)
        (iter (cdr (list 1 2 3 4)) 
              (cons 1
                    null)))

(define (iter (list 1 2 3 4) null)
        (iter (list 2 3 4)
              (cons 1
                    null)))

; 仮パラメタを実引数で置き換える
(if (null? (list 2 3 4))
        (list 1)
        (iter (cdr (list 2 3 4)) 
              (cons (square (car (list 2 3 4)))
                    (list 1))))

(if (null? (list 2 3 4))
        (list 1)
        (iter (cdr (list 2 3 4)) 
              (cons 4
                    (list 1))))

(if (null? (list 3 4))
        (list 4 1)
        (iter (cdr (list 3 4)) 
              (cons 9
                    (list 4 1))))
...
(if (null? '())
        (list 16 9 4 1) ; こちらが評価される
        (iter (cdr '()) 
              (cons (square (car '()))
                    (list 16 9 4 1))))

(square-list (list 1 2 3 4))
; '(16 9 4 1)


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; 仮パラメタを実引数で置き換える
(define (iter (list 1 2 3 4) nil)
    (if (null? (list 1 2 3 4))
        nil
        (iter (cdr (list 1 2 3 4))
              (cons nil
                    (square (car (list 1 2 3 4)))))))

(define (iter (list 1 2 3 4) nil)
    (if (null? (list 1 2 3 4))
        nil
        (iter (cdr (list 1 2 3 4))
              (cons nil
                    (square 1)))))

(define (iter (list 1 2 3 4) nil)
    (if (null? (list 1 2 3 4))
        nil
        (iter (list 2 3 4)
              (cons nil
                    1))))

; 先頭にnilが入る結果となってしまう

(square-list (list 1 2 3 4))
; '((((() . 1) . 4) . 9) . 16)