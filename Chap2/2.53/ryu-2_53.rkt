#lang sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c)
; 予想
; (a b c)
; 実際
; (a b c)

(list (list 'george))
; 予想
; ((george))
; 実際
; ((george))


(cdr '((x1 x2) (y1 y2)))
; 予想
; (y1 y2)
; 実際
; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; 予想
; x2
; 実際
; (y1 y2)

(pair? (car '(a short list)))
; 予想
; #f
; 実際
; #f

(memq 'red '((red shoes) (blue socks)))
; 予想
; (red shoes)
; 実際
; #f

(memq 'red '(red shoes blue socks))
; 予想
; (red shoes blue socks)
; 実際
; (red shoes blue socks)