#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; b.

(define (total-weight mobile)
  (if (pair? mobile)
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))
    mobile))

;; 検算
(define 6weight-mobile
  (make-mobile (make-branch 3 4) (make-branch 1 2)))

(total-weight 6weight-mobile)

;; c.

(define (balanced? mobile)
  (if (pair? mobile)
    (and (= (* (branch-length (left-branch mobile))
               (total-weight (branch-structure (left-branch mobile))))
            (* (branch-length (right-branch mobile))
               (total-weight (branch-structure (right-branch mobile)))))
         (balanced? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (right-branch mobile))))
    #t))

(define balanced-mobile
  (make-mobile
    (make-branch
      6
      (make-mobile (make-branch 1 2) (make-branch 2 1)))
  (make-branch
      2
      (make-mobile (make-branch 4 5) (make-branch 5 4)))))

(balanced? balanced-mobile)

;; d.
;; cons にした場合はcdrで2要素目を取り出すように変更する必要がある。
;; よって
;; (define (right-branch mobile)
;;   (cdr mobile))
;;
;; (define (branch-structure branch)
;;   (cdr branch))
;; と変更すれば動く。
