#lang racket

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(square-list (list 1 2 3 4))

(square-list-2 (list 1 2 3 4))
; -> ((((() . 1) . 4) . 9) . 16)

; square-list
; (cons (square (car things)) answer)
; としていて、thingsはリストの順番で処理されるので、answerの先頭に要素が追加されるので逆順となる

; square-list-w
; answerはリストなのでリストに要素を追加するため結果のようにリストと数値の組み合わせの形となる
