#lang sicp

; 実装方針
; デキューそのものは (cons front rear) の１つ。
; 各ノード（アイテム）は (cons item (cons prev next)) となるような、値と prev/next を保持する 1 ペア
; prev/next はそれぞれ前のノードと次のノードへのポインタとなる
; それぞれの操作は多くてもデキューと対象のノードと前後のノードしか参照しないのでΘ(1)で操作ができる

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue node) (set-car! queue node))
(define (set-rear-ptr! queue node) (set-cdr! queue node))

(define (make-node item) (cons item (cons '() '())))
(define (node-item node) (car node)) ; ノードが保持する値
(define (node-prev node) (cadr node)) ; ノードの前のノード
(define (node-next node) (cddr node)) ; ノードの次のノード
(define (set-node-prev! node prev) (set-car! (cdr node) prev)) ; 前のノードを更新
(define (set-node-next! node next) (set-cdr! (cdr node) next)) ; 次のノードを更新

(define (make-queue) (cons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))

; デキューの先頭要素を取得
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty deque" queue)
      (node-item (front-ptr queue))))

; デキューの最後の要素を取得
(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty deque" queue)
      (node-item (rear-ptr queue))))

; デキューの先頭にノードを追加する
; cond 空だったら: デキューの先頭と末尾のポインタ先を新しいノードにする
; else: 新しいノードの次のノードにこれまでの先頭ノードへ書き換え
;       これまでのノードの前のノードに新しいノードに書き換え
;       デキューの先頭ノードを新しいノードに書き換え
(define (front-insert-queue! queue item)
  (let ((new-node (make-node item)))
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue new-node)
               (set-rear-ptr! queue new-node)
               queue)
        (let ((old-front (front-ptr queue)))
          (set-node-next! new-node old-front)
          (set-node-prev! old-front new-node)
          (set-front-ptr! queue new-node)
          queue))))

; デキューの末尾にノードを追加
; 先頭に追加と逆
(define (rear-insert-queue! queue item)
  (let ((new-node (make-node item)))
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue new-node)
               (set-rear-ptr! queue new-node)
               queue)
        (let ((old-rear (rear-ptr queue)))
          (set-node-prev! new-node old-rear)
          (set-node-next! old-rear new-node)
          (set-rear-ptr! queue new-node)
          queue))))

; デキューの先頭のノードを削除
; cond 空だったら: エラー
; else: デキューの先頭がさすノードを一つ進めるだけ。空だったら '() を設定
(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "FRONT-DELETE! called with an empty deque" queue))
        (else
         (let* ((old-front (front-ptr queue)) ; let* は前の定義を次で使えるようにする
                (next (node-next old-front)))
           (if (null? next)
               (begin (set-front-ptr! queue '())
                      (set-rear-ptr! queue '()))
               (begin (set-node-prev! next '())
                      (set-front-ptr! queue next)))
           queue))))

; デキューの末尾のノードを削除
; 先頭の削除の逆
(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "REAR-DELETE! called with an empty deque" queue))
        (else
         (let* ((old-rear (rear-ptr queue))
                (prev (node-prev old-rear)))
           (if (null? prev)
               (begin (set-front-ptr! queue '())
                      (set-rear-ptr! queue '()))
               (begin (set-node-next! prev '())
                      (set-rear-ptr! queue prev)))
           queue))))
