#lang sicp


(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))



(define (yacht-puzzle)
  (let ((moore-daughter 'mary-ann) ; Mary Ann Moore
        (barnacle-daughter 'melissa) ; Downing大佐所有のMelissaはBarnacle卿の娘の名前をとった → barnacleの娘はmelissa
        (downing-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (hall-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))
        (parker-daughter (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa)))
    
    (require
      (distinct? (list moore-daughter
                       barnacle-daughter
                       downing-daughter
                       hall-daughter
                       parker-daughter)))


    ; どうフィルターすれば良いんだ？？

    (list
     (list 'moore moore-daughter)
     (list 'barnacle barnacle-daughter)
     (list 'downing downing-daughter)
     (list 'hall hall-daughter)
     (list 'parker parker-daughter))))
 
(yacht-puzzle)
