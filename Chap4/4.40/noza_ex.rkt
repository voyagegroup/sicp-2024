#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

; 問題 4.40 拡張版（20人・20階）
;
; let の入れ子で「前の人と同じ階にならない」制約を束縛直後に課す。
; 問題固有の制約は require で並べる。
;
; 4.38/4.39 版との比較:
;   naive: 20^20 ≈ 10^26 通りを生成 → 現実的に不可能
;   本版:  重複を早期除去し 20! ≈ 2.4×10^18 未満に抑える
;          さらに問題固有制約で大幅に枝刈りされる

(define (a-floor)
  (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

(define (multiple-dwelling-20)
  (let ((p1 (a-floor)))
    (require (not (= p1 20)))                      ; p1: 最上階でない
    (let ((p2 (a-floor)))
      (require (not (member p2 (list p1))))
      (require (not (= p2 1)))                     ; p2: 最下階でない
      (let ((p3 (a-floor)))
        (require (not (member p3 (list p1 p2))))
        (require (not (= p3 20)))                  ; p3: 最上階でない
        (require (not (= p3 1)))                   ; p3: 最下階でない
        (require (not (= (abs (- p3 p2)) 1)))      ; p3 と p2 は隣接しない
        (let ((p4 (a-floor)))
          (require (not (member p4 (list p1 p2 p3))))
          (require (> p4 p2))                      ; p4 は p2 より上
          (let ((p5 (a-floor)))
            (require (not (member p5 (list p1 p2 p3 p4))))
            (require (not (= (abs (- p5 p3)) 1)))  ; p5 と p3 は隣接しない
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
                      (let ((p11 (a-floor)))
                        (require (not (member p11 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10))))
                        (let ((p12 (a-floor)))
                          (require (not (member p12 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11))))
                          (let ((p13 (a-floor)))
                            (require (not (member p13 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12))))
                            (let ((p14 (a-floor)))
                              (require (not (member p14 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13))))
                              (let ((p15 (a-floor)))
                                (require (not (member p15 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14))))
                                (let ((p16 (a-floor)))
                                  (require (not (member p16 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15))))
                                  (let ((p17 (a-floor)))
                                    (require (not (member p17 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16))))
                                    (let ((p18 (a-floor)))
                                      (require (not (member p18 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17))))
                                      (let ((p19 (a-floor)))
                                        (require (not (member p19 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18))))
                                        (let ((p20 (a-floor)))
                                          (require (not (member p20 (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19))))
                                          (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
                                                p11 p12 p13 p14 p15 p16 p17 p18 p19 p20))))))))))))))))))))))

(stream->list (in-amb (multiple-dwelling-20)))
