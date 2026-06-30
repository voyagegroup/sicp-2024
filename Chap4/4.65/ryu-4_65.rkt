#lang sicp

; --- 出力

;;; Query input:
; 監督者である人を監督する人を, その機関の「大立物[wheel]」であると宣言する.
(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))

Assertion added to data base.

;;; Query input:
(wheel ?who)


;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

;;; Query input:

; --- 出力

; -- supervisorのデータ
(supervisor (Hacker Alyssa P) (Bitdiddle Ben)) ; フレーム1
(supervisor (Fect Cy D) (Bitdiddle Ben)) ; フレーム2
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
(supervisor (Reasoner Louis) (Hacker Alyssa P))
(supervisor (Bitdiddle Ben) (Warbucks Oliver)) ; フレーム5
(supervisor (Scrooge Eben) (Warbucks Oliver)) ; フレーム6
(supervisor (Cratchet Robert) (Scrooge Eben))
(supervisor (Aull DeWitt) (Warbucks Oliver))
; -- supervisorのデータ

; 1.
; ?person = ?who
(supervisor ?middle-manager ?person)
; 2. 
(supervisor ?x ?middle-manager)

; フレーム1:
; ?middle-manager = Alyssa
; ?person = Ben
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(supervisor ?x (Hacker Alyssa P))
; ?x = (Reasoner Louis)

; 出力1 -> Ben

; フレーム2:
; ?middle-manager = Fect
; ?person = Ben
(supervisor (Fect Cy D) (Bitdiddle Ben))
; ?x = マッチなし

; フレーム5
; ?middle-manager = Ben
; ?person = Warbucks
(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(supervisor ?x (Bitdiddle Ben))
; ?x = Alyssa
; ?x = Fect
; ?x = Tweakit

; 出力2 -> Warbucks
; 出力3 -> Warbucks
; 出力4 -> Warbucks

; フレーム6
; ?middle-manager = Eben
; ?person = Warbucks
(supervisor (Scrooge Eben) (Warbucks Oliver))

(supervisor ?x (Scrooge Eben))
; ?x = Cratchet

; 出力5 -> Warbucks















