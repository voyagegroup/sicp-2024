#lang sicp

#|
Adam
└── Cain
    └── Enoch
        └── Irad
            └── Mehujael
                └── Methushael
                    └── Lamech
                        ├── Jabal
                        └── Jubal



(assert!
 (son Adam Cain))
(assert!
 (son Cain Enoch))
(assert!
 (son Enoch Irad))
(assert!
 (son Irad Mehujael))
(assert!
 (son Mehujael Methushael))
(assert!
 (son Methushael Lamech))
(assert!
 (wife Lamech Ada))
(assert!
 (son Ada Jabal))
(assert!
 (son Ada Jubal))
|#

; Adamの孫
((grandson) Adam Enoch)

; IradはAdamの孫の子[great-grandson]である
((great grandson) Adam Irad)

; 孫の子の子
((great great grandson) Adam Mehujael)

; 4.63
(assert!
 (rule (grandson ?g ?s)
       (and
        (son ?g ?f)
        (son ?f ?s))))
(assert!
 (rule ((grandson) ?g ?s)
       (grandson ?g ?s)))

; リストの最後が語grandsonで終るかどうかを見る規則を書
; 4.62のlast-pair
(assert! (rule (last-pair (?x) (?x))))
(assert!
 (rule (last-pair (?first ?second . ?rest) ?last)
       (last-pair (?second . ?rest) ?last)))


; 今回の

(assert!
 (rule ((great . ?rel) ?x ?y)
       (and
        (last-pair ?rel (grandson)) ; grandsonでおわる
        (?rel ?x ?z) ; ?z が ?x の ?rel 関係
        (son ?z ?y)))) ; ?y が ?z の息子
 ; -> ?y は ?x の (great . ?rel) 関係にある

;;; Query Input:
((great grandson) ?g ?ggs)

;;; Query results:
((great grandson) Irad Lamech)
((great grandson) Enoch Methushael)
((great grandson) Cain Mehujael)
((great grandson) Adam Irad)
