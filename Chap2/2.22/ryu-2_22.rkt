#lang sicp

(define (square x)
  (* x x))


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(square-list (list 1 2 3 4))
; -> (16 9 4 1)


; 手続きを追ってみる
; 手続きを追うためにグローバルに切り出す
(define (iter things answer)
  (if (null? things)
      answer
      (iter (cdr things) 
            (cons (square (car things))
                  answer))))

(iter (list 1 2 3 4) nil)
; (if (null? things)) -> false
(iter (cdr (list 1 2 3 4))
           (cons (square (car (list 1 2 3 4)))
                 nil))
(iter (list 2 3 4)
      (cons (square 1)
            nil))
(iter (list 2 3 4)
      (cons 1 nil))
; 1順完了

(iter (list 2 3 4) (cons 1 nil))
; (if (null? things)) -> false
(iter (cdr (list 2 3 4))
      (cons (square (car (list 2 3 4)))
            (cons 1 nil)))
(iter (list 3 4)
      (cons (square 2)
            (cons 1 nil)))

(iter (list 3 4)
      (cons 4 (cons 1 nil)))
; 2順目完了

; answer が以下のようになっているため
(cons 4 (cons 1 nil))

; ---- 2つ目 ----
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
(square-list-2 (list 1 2 3 4))
; -> ((((() . 1) . 4) . 9) . 16)


(define (iter-2 things answer)
  (if (null? things)
      answer
      (iter-2 (cdr things)
            (cons answer
                  (square (car things))))))
(iter-2 (list 1 2 3 4) nil)

(iter-2 (cdr (list 1 2 3 4))
        (cons nil (square (car (list 1 2 3 4)))))

(iter-2 (list 2 3 4)
        (cons nil (square 1)))
(iter-2 (list 2 3 4)
        (cons nil 1))

; 2順目
(iter-2 (cdr (list 2 3 4))
        (cons (cons nil 1) (square (car (list 2 3 4)))))
(iter-2 (list 3 4)
        (cons (cons nil 1) 4))

; answer が様になっているため
; (cons (cons nil 1) 4))


; 直してみる
; 以下の形になるようにすれば良い
; (cons 1 (cons 4 (cons 9 (cons 16 nil))))

; todo

(define (iter-3 things answer)
  (if (null? things)
      answer
      (iter-3 (cdr things)
            (cons (square (car things)) 
                  (square (car things))))))

(iter-3 (list 1 2 3 4) nil)

(cons nil 1)


















