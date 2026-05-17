
lornaの父はdowing

```racket
#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? li)
  (define (has-dup n li)
    (if (null? li)
        #f
        (if (eq? n (car li))
            #t
            (has-dup n (cdr li)))))
  (define (distinct-itr li)
    (if (null? li)
        #t
        (if (has-dup (car li) (cdr li))
            #f
            (distinct-itr (cdr li)))))
  (distinct-itr li))

(define (yacht-puzzle)
  (let ((moore    (list 'mary 'lorna))
        (barnacle (list 'melissa 'gabrielle)))
    (let ((downing (list (amb 'gabrielle 'lorna 'rosalind) 'melissa)))
      (let ((hall (list (amb 'gabrielle 'lorna 'rosalind) 'rosalind)))
        (require (not (equal? (car downing) (car hall))))
        (require (not (equal? (car hall) (cadr hall))))
        (let ((parker (list (amb 'gabrielle 'lorna 'rosalind) 'mary)))
          (require (distinct? (list (car moore) (car barnacle) (car downing) (car hall) (car parker))))
          (let ((people (list moore barnacle downing hall parker)))
            (require (equal? (cadr (assq 'gabrielle people)) (car parker)))
            (list (list 'moore (car moore))
                  (list 'barnacle (car barnacle))
                  (list 'downing (car downing))
                  (list 'hall (car hall))
                  (list 'parker (car parker)))))))))

(stream->list
 (in-amb
  (yacht-puzzle)))
; '(((moore mary) (barnacle melissa) (downing lorna) (hall gabrielle) (parker rosalind)))
```

maryの父が不明なとき、lornaの父はdowingかparker

```racket
(define (yacht-puzzle)
  (let ((moore    (list (amb 'mary 'gabrielle 'rosalind) 'lorna))
        (barnacle (list 'melissa 'gabrielle)))
    (let ((downing (list (amb 'mary 'gabrielle 'lorna 'rosalind) 'melissa)))
      (let ((hall (list (amb 'mary 'gabrielle 'lorna 'rosalind) 'rosalind)))
        (require (not (equal? (car downing) (car hall))))
        (require (not (equal? (car hall) (cadr hall))))
        (let ((parker (list (amb 'gabrielle 'lorna 'rosalind) 'mary)))
          (require (distinct? (list (car moore) (car barnacle) (car downing) (car hall) (car parker))))
          (let ((people (list moore barnacle downing hall parker)))
            (require (equal? (cadr (assq 'gabrielle people)) (car parker)))
            (list (list 'moore (car moore))
                  (list 'barnacle (car barnacle))
                  (list 'downing (car downing))
                  (list 'hall (car hall))
                  (list 'parker (car parker)))))))))
                  ; '(((moore mary) (barnacle melissa) (downing lorna) (hall gabrielle) (parker rosalind)) ((moore gabrielle) (barnacle melissa) (downing rosalind) (hall mary) (parker lorna)))
```