

```
(assert! (rule (same ?x ?x)))

(assert! (rule (can-replace ?person-1 ?person-2)
      (and (job ?person-1 ?job-1)
           (job ?person-2 ?job-2)
           (or 
            (same ?job-1 ?job-2)
            (can-do-job ?job-1 ?job-2))
           (not (same ?person-1 ?person-2)))))
```

a. Cy D. Fectに代れる人すべて;
```
;;; Query input:
(can-replace ?x (Fect Cy D))

;;; Query results:
(can-replace (Bitdiddle Ben) (Fect Cy D))
(can-replace (Hacker Alyssa P) (Fect Cy D))
```

b. 誰かに代れて, その誰かの方が多くの給料を貰っている人すべてと, 両者の給料.

```
;;; Query input:
(and (salary ?p1 ?s1)
     (salary ?p2 ?s2)
     (can-replace ?p1 ?p2)
     (lisp-value <
 ?s1 ?s2))
;;; Query results:
(and (salary (Aull DeWitt) 25000) (salary (Warbucks Oliver) 150000) (can-replace (Aull DeWitt) (Warbucks Oliver)) (lisp-value < 25000 150000))
(and (salary (Fect Cy D) 35000) (salary (Hacker Alyssa P) 40000) (can-replace (Fect Cy D) (Hacker Alyssa P)) (lisp-value < 35000 40000))
```