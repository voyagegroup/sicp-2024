#lang racket

; make-hasheq キー比較に eq? を使うハッシュテーブル
; visited にカウントした対をキーとして登録してカウントの時に対を数えていたら 0 を返す

(define (count-pairs x)
  (define visited (make-hasheq))
  (define (walk v)
    (cond
      [(not (pair? v)) 0]
      [(hash-has-key? visited v) 0]
      [else (hash-set! visited v #t)
            (+ 1 (walk (car v)) (walk (cdr v)))]))
  (walk x))

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
