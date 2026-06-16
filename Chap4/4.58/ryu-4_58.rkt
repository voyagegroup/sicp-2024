#lang sicp

; ある人がある部門で働くが, その部門で働く監督者を持たなければ, その人をその部門の「黒幕[big shot]」とする規則を定義せよ.

; ある人がある部門で働く
(job ?person (?division . ?rest))

; その部門で働く監督者を持たない

; bossがいる
(supervisor ?person ?boss)

; bossが同じ部門で働いている
(job ?boss (?division . ?boss-rest))

(assert!
 (rule (big-shot ?person ?divition)
       (and
        (job ?person (?division . ?rest))
        (not
         (and
          (supervisor ?person ?boss)
          (job ?boss (?division . ?boss-rest)))))))

; 確認
;;; Query input:
(big-shot ?person ?division)

;;; Query results:
(big-shot (Scrooge Eben) accounting)
(big-shot (Warbucks Oliver) administration)
(big-shot (Bitdiddle Ben) computer)