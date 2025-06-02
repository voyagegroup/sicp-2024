#lang sicp

; 方針
; 内部の方を使うために、scheme-number であればそのままコンテンツを返すようにして、
; それ以外はタグをつけて cons するようにする
; type-tag は数値であれば 'scheme-number を返し、そうでなければ (tag datum) なので car を取れば良い
; contents は数値であればそのまま、そうでなければ cdr を取るようにする

(define (attach-tag type-tag contents)
  (if (= type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((number? datum) 'scheme-number)
    ((pair? datum) (car datum))
    (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
    (cond
        ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))
