#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

; ちょうど一方だけが真
(define (xor a b)
  (and (or a b) (not (and a b))))

(define (liars)
  (let ((betty (amb 1 2 3 4 5)))
    (let ((ethel (amb 1 2 3 4 5)))
      (require (not (member ethel (list betty))))
      (let ((joan (amb 1 2 3 4 5)))
        (require (not (member joan (list betty ethel))))
        (let ((kitty (amb 1 2 3 4 5)))
          (require (not (member kitty (list betty ethel joan))))
          (let ((mary (amb 1 2 3 4 5)))
            (require (not (member mary (list betty ethel joan kitty))))
            (require (xor (= kitty 2) (= betty 3)))
            (require (xor (= ethel 1) (= joan 2)))
            (require (xor (= joan 3)  (= ethel 5)))
            (require (xor (= kitty 2) (= mary 4)))
            (require (xor (= mary 4)  (= betty 1)))
            (list (list 'betty betty)
                  (list 'ethel ethel)
                  (list 'joan  joan)
                  (list 'kitty kitty)
                  (list 'mary  mary))))))))

(stream->list (in-amb (liars)))
