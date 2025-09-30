#lang sicp

; キュー演算の手続き
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

; 先頭のポインタが空リストなら, キューは空だと考える
(define (empty-queue? queue) (null? (front-ptr queue)))

; make-queue構成子は, 初期の空のキューとしてcarとcdrの両方が空リストの対を返す
(define (make-queue) (cons '() '()))

; キューの先頭の項目を選択するには, 先頭を示すポインタで指示した対の carを返す
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

; キューに項目を挿入する
; まずcarが挿入すべき項目で, cdrが空リストである新しい対を作り出す. キューが初めから空なら, キューの先頭と後尾のポインタをこの対へ設定する. そうれなければ, キューの最後の対を, この新しい対を指すように修正し, また後尾ポインタを新しい対へ設定する.
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))


; キューの先頭の項目を削除するには, 先頭のポインタを, 先頭の項の cdrポインタを辿ると見つかるキューの二番目の項目を指すように変更するだけである
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

; 3.21 ここから


(define q1 (make-queue))

(insert-queue! q1 'a)
; ((a) a)

(insert-queue! q1 'b)
; ((a b) b)

(delete-queue! q1)
; ((b) b)

(delete-queue! q1)
; (() b)

'q2ここから

(define (print-queue queue)
  (front-ptr queue))

(define q2 (make-queue))
(insert-queue! q2 'a)
; ((a) a)

(print-queue q2)
; (a)

(insert-queue! q2 'b)
; ((a b) b)

(print-queue q2)
; (a b)

(delete-queue! q2)
; ((b) b)

(print-queue q2)
; (b)

(delete-queue! q2)
; (() b)

(print-queue q2)
; ()
