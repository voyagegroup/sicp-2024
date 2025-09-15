#lang sicp

;; 訪問済み cons セルを `seen` リストで管理し、
;; cdr だけを辿る。再訪したら循環と判定する。
(define (has-cycle? x)
  (define (loop v seen)
    (cond ((not (pair? v)) #f)
          ((memq v seen) #t)
          (else (loop (cdr v) (cons v seen)))))
  (loop x '()))

;; 3.13 から持ってくる ----------------------
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
;; ---------------------------------------

(has-cycle? z)
