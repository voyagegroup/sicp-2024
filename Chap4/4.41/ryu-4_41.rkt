#lang sicp

(define floors '(1 2 3 4 5))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (baker-ok? baker)
  (not (= baker 5)))

(define (cooper-ok? cooper)
  (not (= cooper 1)))

(define (fletcher-ok? fletcher)
  (and (not (= fletcher 1))
       (not (= fletcher 5))))

(define (miller-ok? miller cooper)
  (> miller cooper))

(define (smith-ok? smith fletcher)
  (not (= (abs (- smith fletcher)) 1)))

(define (fletcher-cooper-ok? fletcher cooper)
  (not (= (abs (- fletcher cooper)) 1)))

(define (multiple-dwelling)

  (define (iter baker-list)
    (if (null? baker-list)
        '()
        (append
         (iter-cooper (car baker-list) floors)
         (iter (cdr baker-list)))))

  (define (iter-cooper baker cooper-list)
    (if (null? cooper-list)
        '()
        (let ((cooper (car cooper-list)))
          (append
           (if (and (baker-ok? baker)
                    (cooper-ok? cooper)
                    (distinct? (list baker cooper)))
               (iter-fletcher baker cooper floors)
               '())
           (iter-cooper baker (cdr cooper-list))))))

  (define (iter-fletcher baker cooper fletcher-list)
    (if (null? fletcher-list)
        '()
        (let ((fletcher (car fletcher-list)))
          (append
           (if (and (fletcher-ok? fletcher)
                    (fletcher-cooper-ok? fletcher cooper)
                    (distinct? (list baker cooper fletcher)))
               (iter-miller baker cooper fletcher floors)
               '())
           (iter-fletcher baker cooper (cdr fletcher-list))))))

  (define (iter-miller baker cooper fletcher miller-list)
    (if (null? miller-list)
        '()
        (let ((miller (car miller-list)))
          (append
           (if (and (miller-ok? miller cooper)
                    (distinct? (list baker cooper fletcher miller)))
               (iter-smith baker cooper fletcher miller floors)
               '())
           (iter-miller baker cooper fletcher (cdr miller-list))))))

  (define (iter-smith baker cooper fletcher miller smith-list)
    (if (null? smith-list)
        '()
        (let ((smith (car smith-list)))
          (append
           (if (and (smith-ok? smith fletcher)
                    (distinct?
                     (list baker cooper fletcher miller smith)))
               (list
                (list
                 (list 'baker baker)
                 (list 'cooper cooper)
                 (list 'fletcher fletcher)
                 (list 'miller miller)
                 (list 'smith smith)))
               '())
           (iter-smith baker cooper fletcher miller (cdr smith-list))))))

  (iter floors))

(multiple-dwelling)