```scheme
(define (uniquely-asserted contents frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (let ((result (qeval (content-query contents)
                            (singleton-stream frame))))
       (if (stream-singleton? result)
         result
         the-empty-stream)))
   frame-stream))

(define (stream-singleton? stream)
  (and
   (not (stream-null? stream))
   (stream-null? (stream-cdr stream))))

(define (content-query contents)
  (car contents))

(put 'unique 'qeval uniquely-asserted)
```

```
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(and (supervisor ?employee ?boss)
     (unique (supervisor ?anyone ?boss)))
; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
; (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
```