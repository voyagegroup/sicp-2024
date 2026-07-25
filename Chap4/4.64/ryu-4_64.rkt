#lang sicp


; -- もともと

; ある幹部[staff-person]の監督者がボスであるか, あるいは(再帰的に)その幹部の監督者がボスに身分を越されている[outranked-by]なら, その幹部はその機関においてボスに身分を越されているとする.
(assert! (rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss)))))

;;; Query input:
(outranked-by (Bitdiddle Ben) ?who)


;;; Query results:
(outranked-by (Bitdiddle Ben) (Warbucks Oliver))

;;; Query input:

;; それぞれの質問
; 1.
(supervisor ?staff-person ?boss)
; ?staff-person = (Bitdiddle Ben)
; ?boss = ?who
(suprevisor (Bitdiddle Ben) ?who)


; 2.
(supervisor ?staff-person ?middle-manager)
; ?staff-person = (Bitdiddle Ben)
(supervisor (Bitdiddle Ben) ?middle-manager)

; 3.
; ?boss = ?who
(outranked-by ?middle-manager ?who)

;; 流れ
; フレーム1
; ?who = (Warbucks Oliver)
; ?middle-manager = (Warbucks Oliver)
(outranked-by (Warbucks Oliver) (Warbucks Oliver)) ; → 再帰

; -- Reasoner版
(assert! (rule (outranked-by-2 ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by-2 ?middle-manager ?boss) ; ここと
               (supervisor ?staff-person ?middle-manager))))) ;ここが逆になっている

;;; Query input:
(outranked-by-2 (Bitdiddle Ben) ?who)

;;; Query results:
(outranked-by-2 (Bitdiddle Ben) (Warbucks Oliver))
; 出力はされたが、プロンプトの入力が帰ってこない = ループしている

; boss と middle-managerが束縛されてない状態でoutranked-byが呼ばれてループしている
