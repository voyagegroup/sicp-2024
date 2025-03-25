#lang sicp

; --- 前回までに出てきた手続き

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; --- 今回の手続き
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list (empty-board)) ; memo: empty-boardがオブジェクトになってしまうので、empty-board → (empty-board)にした
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; --- adjoin-position
; 位置の集合に新しい場所の座標を連結する手続き
; rest-of-queens
; 最初のk-1列にk-1個のクィーンを置く方法
; new-row
; k列目にクィーンの置ける行の案


#|
; プリントデバッグ用
(define (adjoin-position  new-row k rest-of-queens)
  (display new-row)
  (newline)
  (display k)
  (newline)
  (display "rest")
  (display rest-of-queens)
  (newline))
|#

(define (adjoin-position  new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

; --- empty-board
; 場所の空集合を表現する
(define (empty-board) '())

; --- safe?
; 他のクィーンに対し, k番目のクィーンが安全な場所を決める
;  (他のクィーンは互いに安全であることが保証されているので, 新しいクィーンが安全なことだけ調べればよい.)
(define (safe? k positions)
  (define current (car positions)) ; 直近でおいたやつ（k番目においた）
  (define previous (cdr positions))
;  (display k)(newline)
;  (display positions)(newline)
;  (display current)(newline)
;  (display previous)(newline)
  (cond ((= k 1) #t) ; 最初は絶対#t
        ((and (safe-row? current previous) ; ななめと横が安全かどうか
              (safe-diagonal? current previous))
         #t)
        (else #f)))

(define (safe-row? current previous)
  (not (member (car current) (map car previous)))) ; (member 2 (list 1 3)) → #f 要素があるかどうかがわかる
(safe-row? (list 3 4) (list (list 1 1) (list 3 2))) ; →#f(行がかぶってる)
(safe-row? (list 5 4) (list (list 1 1) (list 3 2))) ; → #t(安全)

(define (safe-diagonal? current previous)
  (define current-row (car current))
  (define current-col (cdr current))
  (define (conflict? q) ; rowとcolが一致している = 斜めにいる
    (= (abs (- current-row (car q)))
       (abs (- current-col (cdr q)))))
  (null? (filter conflict? previous)))


(define solutions (queens 8))
(length solutions) ; => 92
(car solutions)
; ((4 . 8) (2 . 7) (7 . 6) (3 . 5) (6 . 4) (8 . 3) (5 . 2) (1 . 1))


 












