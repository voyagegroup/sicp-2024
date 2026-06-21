```
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (same ?x ?x)))

(assert! (rule (grandson ?grandpa ?grandson)
      (and (son ?father ?grandson)
           (son ?grandpa ?father))))

(assert! (rule (last-pair (?x) (?x))))

(assert! (rule (last-pair (?li . ?rest) ?last)
      (last-pair ?rest ?last)))

(assert! (rule (end-by-grandson ?li)
    (last-pair ?li (grandson))))

(assert! (rule ((grandson) ?grandpa ?grandson)
               (grandson ?grandpa ?grandson)))

(assert! (rule ((great . ?rel) ?x ?y)
    (and 
        (son ?x ?z)
        (?rel ?z ?y)
        (end-by-grandson ?rel))))
```

```
;;; Query input:
((great grandson) ?g ?ggs)

;;; Query results:
((great grandson) Irad Lamech)
((great grandson) Enoch Methushael)
((great grandson) Cain Mehujael)
((great grandson) Adam Irad)
```

```
;;; Query input:
(?relationship Adam Irad)

;;; Query results:
((great grandson) Adam Irad)
```
