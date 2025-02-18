#lang sicp

(define x (list (list 1 2) (list 3 4)))

x

; 2.18をそのままもってきた
(define (reverse items)
  (define (iter items reversed)
    (if (null? items)
        reversed
        (iter (cdr items) (cons (car items) reversed))))
  (iter items (list)))

(reverse x)
; ((3 4) (1 2))

(define y (list (list 1 2) (list 3 4) (list 5 6)))

(reverse y)
; ((3 4) (1 2))

(define z (list (list 1 2) (list 3 4) (list 5 6 (list 7 8))))

(reverse z)
; ((5 6 (7 8)) (3 4) (1 2))

; (deep-reverse (car (list 1 2)))

; listかどうかを確認するためにからつかう。
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 2))
; -> 2

(count-leaves 1)
; -> 1

(define (deep-reverse items)
  (if (= (count-leaves items) 1) ; おわったあとに、pair?に気がついた
      items
      (reverse (map deep-reverse items))))


(deep-reverse x)
; ((4 3) (2 1))
(deep-reverse y)
; ((6 5) (4 3) (2 1))
(deep-reverse z)
; (((8 7) 6 5) (4 3) (2 1))


