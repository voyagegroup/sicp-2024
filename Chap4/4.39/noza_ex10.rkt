#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

; 問題 4.39 拡張版（10人・10階）
;
; 制約の順序を最適化: O(1) の制約を先に、distinct? を最後に。
; 10人版では distinct? が O(10^2) = O(100) になるため、
; 順序の効果が5人版より大きい。
; それでも 10^10 通りは生成されるため、4.40 の改善には及ばない。

(define (a-floor)
  (amb 1 2 3 4 5 6 7 8 9 10))

(define (multiple-dwelling-10)
  (let ((p1  (a-floor)) (p2  (a-floor)) (p3  (a-floor)) (p4  (a-floor)) (p5  (a-floor))
        (p6  (a-floor)) (p7  (a-floor)) (p8  (a-floor)) (p9  (a-floor)) (p10 (a-floor)))
    ; O(1) の制約を先に
    (require (not (= p1 10)))
    (require (not (= p2 1)))
    (require (not (= p3 10)))
    (require (not (= p3 1)))
    (require (> p4 p2))
    (require (not (= (abs (- p5 p3)) 1)))
    (require (not (= (abs (- p3 p2)) 1)))
    ; distinct? を最後に（O(10^2) = O(100) のコスト）
    (require (distinct? (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)))
    (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)))

; 制約の順序改善で distinct? の呼び出し回数は減るが、
; 生成される組み合わせ数 10^10 は変わらない
; (stream->list (in-amb (multiple-dwelling-10)))
