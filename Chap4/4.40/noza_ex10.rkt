#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

; 問題 4.40 拡張版（10人・10階）
;
; let の入れ子で「前の人と同じ階にならない」制約を束縛直後に課す。
; 問題固有の制約は require で並べる。
;
; 4.38/4.39 版との比較:
;   naive: 10^10 ≈ 10^10 通りを生成 → 遅い
;   本版:  重複を早期除去し 10! ≈ 3.6×10^6 未満に抑える
;          さらに問題固有制約で大幅に枝刈りされる

(define (a-floor)
  (amb 1 2 3 4 5 6 7 8 9 10))

(define (multiple-dwelling-10)
  (let ((p1 (a-floor)))
    (require (not (= p1 10)))                     ; p1: 最上階でない
    (let ((p2 (a-floor)))
      (require (not (member p2 (list p1))))
      (require (not (= p2 1)))                    ; p2: 最下階でない
      (let ((p3 (a-floor)))
        (require (not (member p3 (list p1 p2))))
        (require (not (= p3 10)))                 ; p3: 最上階でない
        (require (not (= p3 1)))                  ; p3: 最下階でない
        (require (not (= (abs (- p3 p2)) 1)))     ; p3 と p2 は隣接しない
        (let ((p4 (a-floor)))
          (require (not (member p4 (list p1 p2 p3))))
          (require (> p4 p2))                     ; p4 は p2 より上
          (let ((p5 (a-floor)))
            (require (not (member p5 (list p1 p2 p3 p4))))
            (require (not (= (abs (- p5 p3)) 1))) ; p5 と p3 は隣接しない
            (let ((p6 (a-floor)))
              (require (not (member p6 (list p1 p2 p3 p4 p5))))
              (let ((p7 (a-floor)))
                (require (not (member p7 (list p1 p2 p3 p4 p5 p6))))
                (let ((p8 (a-floor)))
                  (require (not (member p8 (list p1 p2 p3 p4 p5 p6 p7))))
                  (let ((p9 (a-floor)))
                    (require (not (member p9 (list p1 p2 p3 p4 p5 p6 p7 p8))))
                    (let ((p10 (a-floor)))
                      (require (not (member p10 (list p1 p2 p3 p4 p5 p6 p7 p8 p9))))
                      (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10))))))))))))

(stream->list (in-amb (multiple-dwelling-10)))
