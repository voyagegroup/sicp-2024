```
(assert!
 (rule (big-shot ?person ?division)
   (and (job ?person (?division . ?type))
        (not (and (supervisor ?person ?boss)
                  (job ?boss (?division . ?boss-type)))))))
```

```
;;; Query input:
(big-shot (Fect Cy D) computer)

;;; Query results:

;;; Query input:
(big-shot (Bitdiddle Ben) computer)

;;; Query results:
(big-shot (Bitdiddle Ben) computer)
```