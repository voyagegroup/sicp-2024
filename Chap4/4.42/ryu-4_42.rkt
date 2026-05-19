#lang sicp

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

; 片方t,片方fの場合true
(define (xor a b)
  (or
   (and (eq? #t a) (eq? #f b))
   (and (eq? #f a) (eq? #t b))))

(define (usotsuki)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    

    (require
      (distinct? (list betty ethel joan kitty mary)))

    ; betty
    (require (xor
              (= kitty 2)
              (= betty 3)))

    ; ethel
    (require (xor
              (= ethel 1)
              (= joan 2)))

    ; joan
    (require (xor
              (= joan 3)
              (= ethel 5)))

    ; kitty
    (require (xor
              (= kitty 2)
              (= mary 4)))

    ;  mary
    (require (xor
              (= mary 4)
              (= betty 1)))

    (list
     (list 'betty betty)
     (list 'ethel ethel)
     (list 'joan joan)
     (list 'kitty kitty)
     (list 'mary mary))
              

    ))

(usotsuki)
; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))