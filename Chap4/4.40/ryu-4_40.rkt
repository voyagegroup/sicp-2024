#lang sicp

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))


; 元の実装だと、たとえば、1, 1, 2, 3, 5 みたいな場合、1, 1の時点でだめなのに、最後までみる必要がある
; なので、1, 1の時点でバックトラックする必要がある。
; letをネストさせて、各ネストで` (require (not (= baker cooper)))` の様な同階層にいるかのチェックをする
; くわえて、fletcherは1階にはいないというような条件は、ambの初期化の時から1をかかないようにした

(define (multiple-dwelling-fast)
  (let ((baker (amb 1 2 3 4)))
    (let ((cooper (amb 2 3 4 5)))
      (require (not (= baker cooper)))

      (let ((fletcher (amb 2 3 4)))
        (require (not (= fletcher baker)))
        (require (not (= fletcher cooper)))
        (require (not (= (abs (- fletcher cooper)) 1)))

        (let ((miller (amb 1 2 3 4 5)))
          (require (not (= miller baker)))
          (require (not (= miller cooper)))
          (require (not (= miller fletcher)))
          (require (> miller cooper))

          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= smith baker)))
            (require (not (= smith cooper)))
            (require (not (= smith fletcher)))
            (require (not (= smith miller)))
            (require (not (= (abs (- smith fletcher)) 1)))

            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))


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
