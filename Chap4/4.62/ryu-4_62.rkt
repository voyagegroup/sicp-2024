#lang sicp
; 問題2.17の, 空でないリストの最後の要素を含むリストを返すlast-pair演算を実装する規則を定義せよ. (last-pair (3) ?x), (last-pair (1 2 3) ?x)および(last-pair (2 ?x) (3))のような質問で規則をチェックせよ. その規則は(last-pair ?x (3))のような質問にも正しく働くか.

; ---- 2.17
; https://sicp.iijlab.net/fulltext/x221.html
; https://github.com/voyagegroup/sicp-2024/blob/main/Chap2/2.17/ryu-2_17.rkt

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))
  

(last-pair (list 23 72 149 34))
; -> 34

(last-pair (list 23 72 149))
; -> 149

; ----- 2.17

;;; Query input:
(assert! (rule (last-pair (?x) (?x))))
               
(assert!
 (rule (last-pair (?first ?second . ?rest) ?last)
       (last-pair (?second . ?rest) ?last)))

Assertion added to data base.

;;; Query input:

Assertion added to data base.

;;; Query input:
(last-pair (3) ?x)

;;; Query results:
(last-pair (3) (3))

;;; Query input:
(last-pair (1 2 3) ?x)

;;; Query results:
(last-pair (1 2 3) (3))

;;; Query input:
(last-pair (2 ?x) (3))

;;; Query results:
(last-pair (2 3) (3))

;;; Query input:

; --

;;; Query input:
(last-pair ?x (3))

;;; Query results:

; 入力欄がおかしくなった
