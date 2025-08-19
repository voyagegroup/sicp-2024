#lang sicp


(define f
  (let ((first-time? #t))  ; 最初は#t(真)
    (lambda (x)
      (if first-time?
          (begin
            (set! first-time? #f) ; #f(偽)に変更
            x) ; 引数をそのまま返す
          0)))) ; 2回目以降は0を返す

(+ (f 0) (f 1))

; 左から
; (f 0) -> 0
; (f 1) -> 0

; 右から
; (f 1) -> 1
; (f 0) -> 0