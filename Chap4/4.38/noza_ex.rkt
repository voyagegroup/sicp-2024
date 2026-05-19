#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

; 問題 4.38 拡張版（20人・20階）
;
; 元のパズルを20人に拡張。
; p1-p5 が元の Baker-Smith に対応し制約を持つ。
; p6-p20 は distinct 制約のみ。
; 元の smith-fletcher 隣接制約（p5-p3）は除去済み。
;
; 組み合わせ数: 20^20 ≈ 10^26 (distinct? 前)
;              20!   ≈ 2.4×10^18 (distinct? 後)
; → 現実的な時間では終わらない。4.40 の改善が必要なことを示す。

(define (a-floor)
  (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(define (multiple-dwelling-20)
  (let ((p1  (a-floor)) (p2  (a-floor)) (p3  (a-floor)) (p4  (a-floor)) (p5  (a-floor))
        (p6  (a-floor)) (p7  (a-floor)) (p8  (a-floor)) (p9  (a-floor)) (p10 (a-floor))
        (p11 (a-floor)) (p12 (a-floor)) (p13 (a-floor)) (p14 (a-floor)) (p15 (a-floor))
        (p16 (a-floor)) (p17 (a-floor)) (p18 (a-floor)) (p19 (a-floor)) (p20 (a-floor)))
    (require (distinct? (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
                              p11 p12 p13 p14 p15 p16 p17 p18 p19 p20)))
    ; p1 (baker)   : 最上階でない
    (require (not (= p1 20)))
    ; p2 (cooper)  : 最下階でない
    (require (not (= p2 1)))
    ; p3 (fletcher): 最上階でも最下階でもない
    (require (not (= p3 20)))
    (require (not (= p3 1)))
    ; p4 (miller)  : p2 (cooper) より上
    (require (> p4 p2))
    ; p5-p3 隣接制約は除去（問題 4.38）
    ; p3 (fletcher) と p2 (cooper) は隣接しない
    (require (not (= (abs (- p3 p2)) 1)))
    (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
          p11 p12 p13 p14 p15 p16 p17 p18 p19 p20)))

; 警告: このバージョンは現実的な時間で完了しない
(stream->list (in-amb (multiple-dwelling-20)))
