#lang sicp

(define (value-ptr ptr) (caar ptr))
(define (prev-ptr ptr) (cdar ptr))
(define (next-ptr ptr) (cdr ptr))


(define (make-ptr value prev next)
  (cons (cons value prev) next))

(define (set-prev-ptr! ptr node)
  (set-cdr! (car ptr) node))
(define (set-next-ptr! ptr node)
  (set-cdr! ptr node))


(define (make-deque)
  (cons '() '()))


(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque)
  (null? (front-ptr deque)))


(define (front-insert-queue! deque item)
  (let ((new-node (make-ptr item '() (front-ptr deque))))
    (cond ((empty-deque? deque)
           ; 空の場合: 両ポインタを新ノードにする
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           ; 既存の先頭のprevを新ノードに設定
           (set-prev-ptr! (front-ptr deque) new-node)
           ; front-ptrを新ノードに更新
           (set-front-ptr! deque new-node)))))

(define (rear-insert-queue! deque item)
  (let ((new-node (make-ptr item (rear-ptr deque) '())))
    (cond ((empty-deque? deque)
           ; 空の場合
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           ; 既存の末尾のnextを新ノードに設定
           (set-next-ptr! (rear-ptr deque) new-node)
           ; rear-ptrを新ノードに更新
           (set-rear-ptr! deque new-node)))))

(define (front-delete-queue! deque)
  (cond ((empty-deque? deque)
         ; 空のデキューからの削除はエラー
         (error "DELETE! called with an empty deque"))
        (else
         ; front-ptrを次のノードに移動
         (set-front-ptr! deque (next-ptr (front-ptr deque)))
         (if (null? (front-ptr deque))
             ; 最後の要素を削除した場合: rear-ptrもクリア
             (set-rear-ptr! deque '())
             ; 新しい先頭のprevをnilに設定
             (set-prev-ptr! (front-ptr deque) '())))))

(define (rear-delete-queue! deque)
  (cond ((empty-deque? deque)
         ; 空のデキューからの削除はエラー
         (error "DELETE! called with an empty deque"))
        (else
         ; rear-ptrを前のノードに移動
         (set-rear-ptr! deque (prev-ptr (rear-ptr deque)))
         (if (null? (rear-ptr deque))
             ; 最後の要素を削除した場合: front-ptrもクリア
             (set-front-ptr! deque '())
             ; 新しい末尾のnextをnilに設定
             (set-next-ptr! (rear-ptr deque) '())))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque")
      (value-ptr (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque")
      (value-ptr (rear-ptr deque))))

(define (print-deque deque)
  (define (print-list node)
    (if (null? node)
        '()
        (cons (value-ptr node) 
              (print-list (next-ptr node)))))
  (print-list (front-ptr deque)))



; -- 動作検証 ---
(define dq (make-deque))


; 前に入れる
(front-insert-queue! dq 'b)
(print-deque dq)
; (b)
(front-insert-queue! dq 'a)
(print-deque dq)
; (a)

; 後ろに入れる
(rear-insert-queue! dq 'c)
(print-deque dq)
; (a b c)

; 前のあたいの確認
(display (front-deque dq))
; a
(newline)


(display (rear-deque dq))
; c

(newline)

; 先頭削除
(front-delete-queue! dq)
(print-deque dq)
; (b c)

; 後ろ削除
(rear-delete-queue! dq)
(print-deque dq)
; (b)


















