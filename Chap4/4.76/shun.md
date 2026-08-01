
```
(define (merge-frames frame1 frame2)
  (if (null? frame2)
      frame1
      (let* ((binding (car frame2))
             (result (unify-match (binding-variable binding) (binding-value binding) frame1)))
        (if (eq? result 'failed)
            'failed
            (merge-frames result (cdr frame2))))))

(define (merge-frame-stream frame-stream1 frame-stream2)
  (stream-flatmap
   (lambda (frame1)
     (stream-flatmap
      (lambda (frame2)
        (let ((result (merge-frames frame1 frame2)))
          (if (eq? result 'failed)
              the-empty-stream
              (singleton-stream result))))
        frame-stream2))
   frame-stream1))

(define (new-conjoin conjuncts frame-stream)
  (if (null? conjuncts)
      frame-stream
      (stream-flatmap
       (lambda (frame)
         (let ((initial-stream (singleton-stream frame)))
           (merge-frame-stream
             (qeval (first-conjunct conjuncts) initial-stream)
             (new-conjoin (rest-conjuncts conjuncts)
                    initial-stream))))
       frame-stream)))
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
```

```
(and (supervisor ?employee ?boss)
     (supervisor ?boss (Warbucks Oliver)))
```

```
(and (supervisor (Cratchet Robert) (Scrooge Eben)) (supervisor (Scrooge Eben) (Warbucks Oliver)))
(and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(and (supervisor (Fect Cy D) (Bitdiddle Ben)) (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
```