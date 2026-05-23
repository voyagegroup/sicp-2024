#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

; 問題 4.38 拡張版（10人・10階）
;
; 元のパズルを10人に拡張。
; p1-p5 が元の Baker-Smith に対応し制約を持つ。
; p6-p10 は distinct 制約のみ。
; 元の smith-fletcher 隣接制約（p5-p3）は除去済み。
;
; 組み合わせ数: 10^10 ≈ 10^10 (distinct? 前)
;              10!   ≈ 3.6×10^6 (distinct? 後)

(define (a-floor)
  (amb 1 2 3 4 5 6 7 8 9 10))

(define (multiple-dwelling-10)
  (let ((p1  (a-floor)) (p2  (a-floor)) (p3  (a-floor)) (p4  (a-floor)) (p5  (a-floor))
        (p6  (a-floor)) (p7  (a-floor)) (p8  (a-floor)) (p9  (a-floor)) (p10 (a-floor)))
    (require (distinct? (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)))
    ; p1 (baker)   : 最上階でない
    (require (not (= p1 10)))
    ; p2 (cooper)  : 最下階でない
    (require (not (= p2 1)))
    ; p3 (fletcher): 最上階でも最下階でもない
    (require (not (= p3 10)))
    (require (not (= p3 1)))
    ; p4 (miller)  : p2 (cooper) より上
    (require (> p4 p2))
    ; p5-p3 隣接制約は除去（問題 4.38）
    ; p3 (fletcher) と p2 (cooper) は隣接しない
    (require (not (= (abs (- p3 p2)) 1)))
    (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)))

(stream->list (in-amb (multiple-dwelling-10)))
