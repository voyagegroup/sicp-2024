#lang sicp

(#%require (file "../0.0/noza-question.rkt"))

(define the-empty-stream '())

(define (unique-query operands)
  (car operands))

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result-stream
            (qeval (unique-query operands)
                   (singleton-stream frame))))
       (if (and (not (stream-null? result-stream))
                (stream-null? (stream-cdr result-stream)))
           result-stream
           the-empty-stream)))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)

(define supervisor-assertions
  '((supervisor (Hacker Alyssa P) (Bitdiddle Ben))
    (supervisor (Fect Cy D) (Bitdiddle Ben))
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    (supervisor (Reasoner Louis) (Hacker Alyssa P))
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))
    (supervisor (Scrooge Eben) (Warbucks Oliver))
    (supervisor (Cratchet Robert) (Scrooge Eben))
    (supervisor (Aull DeWitt) (Warbucks Oliver))))

(for-each add-rule-or-assertion! supervisor-assertions)

(query-driver-loop)

; query
; (and (supervisor ?person ?supervisor)
;         (unique (supervisor ?anyone ?supervisor)))

;;; Query input:
; (and (supervisor ?person ?supervisor)
;         (unique (supervisor ?anyone ?supervisor)))

;;; Query results:
; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
; (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
