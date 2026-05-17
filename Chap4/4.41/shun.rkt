#lang racket


(define (multiple-dwelling)
  (define (valid? floors)
    (let ((baker (list-ref floors 0))
          (cooper (list-ref floors 1))
          (fletcher (list-ref floors 2))
          (miller (list-ref floors 3))
          (smith (list-ref floors 4)))
      (and (not (= baker 5))
           (not (= cooper 1))
           (not (= fletcher 5))
           (not (= fletcher 1))
           (> miller cooper)
           (not (= (abs (- smith fletcher)) 1))
           (not (= (abs (- fletcher cooper)) 1)))))
  (filter valid? (permutations '(1 2 3 4 5))))

; permutations はリストの全順列を返す関数

(multiple-dwelling)
; '((3 2 4 5 1))