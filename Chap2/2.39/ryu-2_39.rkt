#lang sicp

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (op (car rest) (iter result (cdr rest)))))
  (iter initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(reverse-r (list 1 2 3))
; x: 1, y: (list 2 3)
; x: 2, y: (list 3)
; x: 3, y: (list)

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse-l (list 1 2 3))
; (iter (cons nil (car (list 1 2 3)) (cdr (list 1 2 3))
; (iter (cons 1 nil) (list 2 3))
; (iter (cons 2 (cons 1 nil)) (list 3))
; (iter (cons 3 (cons 2 (cons 1 nil))) (list))
; (cons 3(cons 2 (cons 1 nil)))
; -> (3 2 1)