#lang sicp


(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch-1 (make-branch 1 10))

(define right-branch-1 (make-branch 1 20))
(define right-branch-2 (make-branch 2 right-branch-1))

(define mobile (make-mobile left-branch-1 right-branch-2))
mobile
; ((1 10) (2 (1 20))) 

; a. これに対応する選択子(モービルの枝を返す)left-branchと right-branchと, (枝の部品を返す)branch-lengthと branch-structureを書け.

; 枝を返す
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(left-branch mobile)
; (1 10)
(right-branch mobile)
; (2 (1 20))

; 枝の部品を返す
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(branch-length (left-branch mobile))
; 1
(branch-structure (left-branch mobile))
; 10

(branch-length (right-branch mobile))
; 2
(branch-structure (right-branch mobile))
; (1 20)


; b. この選択子を使い, モービルの全重量を返す手続きtotal-weightを定義せよ.

(define (total-weight mobile)
  (define (iter branch)
    (if (not (pair? branch))
        branch
        (iter (branch-structure branch))))
  (+ (iter (left-branch mobile)) (iter (right-branch mobile))))

(total-weight mobile)
; -> 30        

; c. モービルは最上段左の枝による回転力と, 最上段右の枝による回転力が等しく, (つまり左の棒の長さと棒の左にかかっている重さを掛けたものが右側の対応する積に等しく,) しかも枝にぶら下っている部分モービルのそれぞれが釣合っている時, 釣合っている(balanced)という. 二進モービルが釣合っているかどうかをテストする述語を設計せよ.

; branch内のlengthの合計をだす
(define (branch-total-length branch)
  (define (iter branch total)
    (if (not (pair? branch))
        total
        (iter (branch-structure branch) (+ total (branch-length branch)))))
  (iter branch 0))

(branch-total-length (left-branch mobile))
; -> 1
(branch-total-length (right-branch mobile))
; -> 3

; branchのweightを返す（bのiterコピペ）
(define (branch-weight branch)
    (if (not (pair? branch))
        branch
        (branch-weight (branch-structure branch))))

; branchのトルクを返す
(define (branch-torque branch)
  (* (branch-weight branch) (branch-total-length branch)))
(branch-torque (left-branch mobile))
; -> 10
(branch-torque (right-branch mobile))
; -> 60

(define (balanced? mobile)
  (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile))))

(balanced? mobile)
; #f


(define left-branch-2 (make-branch 6 10))
(define balanced-mobile (make-mobile left-branch-2 right-branch-2))
(balanced? balanced-mobile)
; #t
















  
