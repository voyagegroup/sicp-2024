#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

; 問題 4.38
; smith と fletcher が隣の階に住まないという制約を除去した場合の解の数を求める

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ; smith-fletcher の隣接制約を除去
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker    baker)
          (list 'cooper   cooper)
          (list 'fletcher fletcher)
          (list 'miller   miller)
          (list 'smith    smith))))

(stream->list (in-amb (multiple-dwelling)))
