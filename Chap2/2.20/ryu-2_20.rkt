#lang sicp

(define (f . list)
  (display list))

; (f 1 2 3 4)
; -> (1 2 3 4)

(define (same-parity x . parity)
  (define (filter l filtered)
    (cond ((null? l) filtered); lがnullなら処理を終わりにする
          ((if (= (remainder x 2) (remainder (car l) 2)) ; % 2 の結果が同じかをみる
               (filter (cdr l) (append filtered (list (car l)))) ; 同じならfilteredに追加する
               (filter (cdr l) filtered))))) ; 同じでないなら、そのままfilteredを返す
  (filter parity (list x)))

(same-parity 1 2 3 4 5 6 7)
; -> (1 3 5 7)


(same-parity 2 3 4 5 6 7)
; -> (2 4 6)


(same-parity 123 456 789 101112)
; (123 789)
