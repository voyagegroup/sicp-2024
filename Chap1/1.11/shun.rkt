#lang racket

;再帰
(define (rec_f n)
  (if (< n 3)
      n
      (+ (rec_f (- n 1))
        (+ (* 2 (rec_f (- n 2)))
            (* 3 (rec_f (- n 3))))
      )
  )
)

; 反復
(define (iter_f n) (if (< n 0); 負の場合はそのまま返す
                        n
                        (rr n 0 1 2)
                    ))
(define (rr max f0 f1 f2)
  (cond
    ((= max 0) f0)
    ((= max 1) f1)
    ((= max 2) f2)
    (else (rr (- max 1) f1 f2 (+ f2 (+ (* 2 f1) (* 3 f0))))));以前の値を使いまわしつつ、最大の値が計算されて増えていくイメージ
  )