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
  (let ((cooper (amb 2 3 4 5))) ;制約をそのまま表現
    (let ((miller (amb 1 2 3 4 5)))
      (require (> miller cooper))
      (let ((fletcher (amb 2 3 4)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((smith (amb 1 2 3 4 5)))
          (require (not (= (abs (- smith fletcher)) 1)))
          (let ((baker (amb 1 2 3 4)))
            (require
              (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))


(stream->list
 (in-amb
  (multiple-dwelling)))
; '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))