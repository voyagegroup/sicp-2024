#lang sicp

; https://github.com/voyagegroup/sicp-2024/blob/main/Chap2/2.42/ryu-2_42.rkt

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (eight-queen)
  (let ((q1 (amb 1 2 3 4 5 6 7 8))
        (q2 (amb 1 2 3 4 5 6 7 8))
        (q3 (amb 1 2 3 4 5 6 7 8))
        (q4 (amb 1 2 3 4 5 6 7 8))
        (q5 (amb 1 2 3 4 5 6 7 8))
        (q6 (amb 1 2 3 4 5 6 7 8))
        (q7 (amb 1 2 3 4 5 6 7 8))
        (q8 (amb 1 2 3 4 5 6 7 8)))

    ; 同じ行禁止
    (require
     (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8)))

    ; 斜め禁止
    (require (not (= (abs (- q1 q2)) 1)))
    (require (not (= (abs (- q1 q3)) 2)))
    (require (not (= (abs (- q1 q4)) 3)))
    (require (not (= (abs (- q1 q5)) 4)))
    (require (not (= (abs (- q1 q6)) 5)))
    (require (not (= (abs (- q1 q7)) 6)))
    (require (not (= (abs (- q1 q8)) 7)))

    (require (not (= (abs (- q2 q3)) 1)))
    (require (not (= (abs (- q2 q4)) 2)))
    (require (not (= (abs (- q2 q5)) 3)))
    (require (not (= (abs (- q2 q6)) 4)))
    (require (not (= (abs (- q2 q7)) 5)))
    (require (not (= (abs (- q2 q8)) 6)))

    (require (not (= (abs (- q3 q4)) 1)))
    (require (not (= (abs (- q3 q5)) 2)))
    (require (not (= (abs (- q3 q6)) 3)))
    (require (not (= (abs (- q3 q7)) 4)))
    (require (not (= (abs (- q3 q8)) 5)))

    (require (not (= (abs (- q4 q5)) 1)))
    (require (not (= (abs (- q4 q6)) 2)))
    (require (not (= (abs (- q4 q7)) 3)))
    (require (not (= (abs (- q4 q8)) 4)))

    (require (not (= (abs (- q5 q6)) 1)))
    (require (not (= (abs (- q5 q7)) 2)))
    (require (not (= (abs (- q5 q8)) 3)))

    (require (not (= (abs (- q6 q7)) 1)))
    (require (not (= (abs (- q6 q8)) 2)))

    (require (not (= (abs (- q7 q8)) 1)))

    (list q1 q2 q3 q4 q5 q6 q7 q8)))

(eight-queen)
; (1 5 8 6 3 7 2 4)