#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

; 問題 4.39 拡張版（20人・20階）
;
; 制約の順序を最適化: O(1) の制約を先に、distinct? を最後に。
; 20人版では distinct? が O(20^2) = O(400) になるため、
; 順序の効果が5人版より大きい。
; それでも 20^20 通りは生成されるため、4.40 の改善には及ばない。

(define (a-floor)
  (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(define (multiple-dwelling-20)
  (let ((p1  (a-floor)) (p2  (a-floor)) (p3  (a-floor)) (p4  (a-floor)) (p5  (a-floor))
        (p6  (a-floor)) (p7  (a-floor)) (p8  (a-floor)) (p9  (a-floor)) (p10 (a-floor))
        (p11 (a-floor)) (p12 (a-floor)) (p13 (a-floor)) (p14 (a-floor)) (p15 (a-floor))
        (p16 (a-floor)) (p17 (a-floor)) (p18 (a-floor)) (p19 (a-floor)) (p20 (a-floor)))
    ; O(1) の制約を先に
    (require (not (= p1 20)))
    (require (not (= p2 1)))
    (require (not (= p3 20)))
    (require (not (= p3 1)))
    (require (> p4 p2))
    (require (not (= (abs (- p5 p3)) 1)))
    (require (not (= (abs (- p3 p2)) 1)))
    ; distinct? を最後に（O(20^2) = O(400) のコスト）
    (require (distinct? (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
                              p11 p12 p13 p14 p15 p16 p17 p18 p19 p20)))
    (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
          p11 p12 p13 p14 p15 p16 p17 p18 p19 p20)))

; 警告: このバージョンも現実的な時間で完了しない
; 制約の順序改善で distinct? の呼び出し回数は減るが、
; 生成される組み合わせ数 20^20 は変わらない
; (stream->list (in-amb (multiple-dwelling-20)))
