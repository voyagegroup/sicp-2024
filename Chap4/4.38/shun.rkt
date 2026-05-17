#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? li)
  (define (has-dup n li)
    (if (null? li)
        #f
        (if (= n (car li))
            #t
            (has-dup n (cdr li)))))
  (define (distinct-itr li)
    (if (null? li)
        #t
        (if (has-dup (car li) (cdr li))
            #f
            (distinct-itr (cdr li)))))
  (distinct-itr li))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ;(require (not (= (abs (- smith fletcher)) 1))) コメントアウト
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


(stream->list
 (in-amb
  (multiple-dwelling)))

;'(((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
;  ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
;  ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
;  ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
;  ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1)))