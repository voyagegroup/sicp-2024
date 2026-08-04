#lang sicp

; notは、0こなら#t、1以上なら#fだった
; (stream-null?) で判断していた
; uniqueは、0 #f, 1 #t, 2以上 #f
; (stream-null? (stream-cdr result)) で2こ目がnullかで判定をする

; nagate
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)


; unique
(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result (qeval (car operands) (singleton-stream frame))))
       (if (singleton-stream? result)
           result
           the-empty-stream)))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)


#|
動作検証

;;; Query input:
(assert! (job (Bitdiddle Ben) (computer wizard)))

Assertion added to data base.

;;; Query input:
(assert! (job (Hacker Alyssa) (computer programmer)))

Assertion added to data base.

;;; Query input:
(assert! (job (Fect Cy D) (computer programmer)))

Assertion added to data base.

;;; Query input:
(assert! (job (Reasoner Louis) accounting))

Assertion added to data base.

;;; Query input:
(unique (job ?x (computer wizard)))

;;; Query results:
(unique (job (Bitdiddle Ben) (computer wizard)))

;;; Query input:
(unique (job ?x (computer programmer)))

;;; Query results:

;;; Query input:
(and (job ?x ?j) (unique (job ?anyone ?j)))


;;; Query results:
(and (job (Reasoner Louis) accounting) (unique (job (Reasoner Louis) accounting)))
(and (job (Bitdiddle Ben) (computer wizard)) (unique (job (Bitdiddle Ben) (computer wizard))))

;;; Query input:
|#
