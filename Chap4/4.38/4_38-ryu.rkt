#lang sicp

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))


(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5))) ; Bakerは最上階に住むのではない
        (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= cooper 1))) ; Cooperは最下階に住むのではない
    (require (not (= fletcher 5))) ; Fletcherは最上階にも最下階にも住むのではない
    (require (not (= fletcher 1)))
    (require (> miller cooper)) ; MillerはCooperより上の階に住んでいる
    ; (require (not (= (abs (- smith fletcher)) 1))) ; SmithはFletcherの隣の階に住むのではない.
    (require (not (= (abs (- fletcher cooper)) 1))) ; FletcherはCooperの隣の階に住むのではない
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


(multiple-dwelling)


(amb)
(amb)
(amb)
(amb)
(amb)
(amb)

#|
((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
amb tree exhausted
>

A. 5通り
|#
