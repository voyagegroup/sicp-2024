#lang racket

(display (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

(newline)

(display (car (list (list 7))))

(newline)

(display (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

(newline)

(display (cdr (list 5 (list 6 7))))
; ((6 7))
; ここから取り出すにはcarを使う必要があった