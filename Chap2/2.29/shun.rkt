#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; b
(define weight5-mobile
  (make-mobile (make-branch 1 2) (make-branch 1 3)))

(define weight10-mobile
  (make-mobile (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 5))) (make-branch 1 3)))

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile)))))
  )

(total-weight weight5-mobile)
(total-weight weight10-mobile)

; c
(define (balanced mobile)
  (if (not (pair? mobile))
      #t
      (and
       (= (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
         (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile)))))
           (balanced (branch-structure (left-branch mobile)))
           (balanced (branch-structure (right-branch mobile)))
      )))

(define weight6-mobile
  (make-mobile (make-branch 1 3) (make-branch 1 3)))

 (balanced weight6-mobile)

; d
;(define (make-mobile left right)
;  (cons left right))

;(define (make-branch length structure)
;  (cons length structure))

; の時、

;(define (right-branch mobile)
;  (cdr mobile))

;(define (branch-structure branch)
;  (cdr branch))

; とすればプログラムは今まで通りに動作する。
; たったこれだけの変更ですむ。