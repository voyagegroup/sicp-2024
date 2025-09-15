#lang sicp

; 訪問済みの cons セル（対）を `visited` リストで管理し、
; 再訪したペアは数えない。`memq` により同一性(eq?)で判定する。

(define (count-pairs x)
  (let ((visited '()))
    (define (walk v)
      (cond ((not (pair? v)) 0)
            ((memq v visited) 0)
            (else (set! visited (cons v visited))
                  (+ 1 (walk (car v)) (walk (cdr v))))))
    (walk x)))

;; 3.16 で作った構造
(define list-three (list 'a 'b 'c))

(define list-a (cons 'a '()))
(define list-aa (cons list-a '()))
(define list-four (cons list-aa list-a))

(define list-b (cons 'b '()))
(define list-bb_bb (cons list-b list-b))
(define list-seven (cons list-bb_bb list-bb_bb))


(count-pairs list-three)
(count-pairs list-four)
(count-pairs list-seven)
