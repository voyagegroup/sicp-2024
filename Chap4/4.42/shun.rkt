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

(define (lier-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

(stream->list
 (in-amb
  (lier-puzzle)))
; '(((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4)))