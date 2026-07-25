#lang sicp


(sum ?amount
     (and (job ?x (computer programmer))
          (salary ?x ?amount)))

; フレーム1
; ?x = Alyssa
(job (Hacker Alyssa P) (computer programmer))
; ?amount = 40000
(salary (Hacker Alyssa P) 40000)

; フレーム2
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)


; ...
; ?amountの出力ストリームができる
; これをsumして出力

; wheel の合計を求めるためにこうなる
(sum ?amount
     (and (wheel ?x)
          (salary ?x ?amount)))

; wheelは重複して同じひとがでる。
; 4.65では以下の出力がでたので、(Warbucks * 4) + Bitdiddle の給料が出力される
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

; sqlみたいな distinct を用意すると打開できる
