#|
a. Ben Bitdiddleが監督している人すべての名前とその住所;

;;; Query input:
(and
  (supervisor ?x (Bitdiddle Ben))
  (address ?x ?y))

;;; Query results:
(and (supervisor (Tweakit Lem E) (Bitdiddle Ben)) (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(and (supervisor (Fect Cy D) (Bitdiddle Ben)) (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(and (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))

b. 給料がBen Bitdiddleのそれより少ない人のすべてと, その人たちの給料と, Ben Bitdiddleの給料;
(and
  (salary (Bitdiddle Ben) ?ben-salary)
  (salary ?person ?person-salary)
  (lisp-value < ?person-salary ?ben-salary))

;;; Query results:proc:(primitive #<procedure:<>)
args(25000 60000)

(and (salary (Bitdiddle Ben) 60000) (salary (Aull DeWitt) 25000) (lisp-value < 25000 60000))proc:(primitive #<procedure:<>)
args(18000 60000)

(and (salary (Bitdiddle Ben) 60000) (salary (Cratchet Robert) 18000) (lisp-value < 18000 60000))proc:(primitive #<procedure:<>)
args(75000 60000)
proc:(primitive #<procedure:<>)
args(150000 60000)
proc:(primitive #<procedure:<>)
args(30000 60000)

(and (salary (Bitdiddle Ben) 60000) (salary (Reasoner Louis) 30000) (lisp-value < 30000 60000))proc:(primitive #<procedure:<>)
args(25000 60000)

(and (salary (Bitdiddle Ben) 60000) (salary (Tweakit Lem E) 25000) (lisp-value < 25000 60000))proc:(primitive #<procedure:<>)
args(35000 60000)

(and (salary (Bitdiddle Ben) 60000) (salary (Fect Cy D) 35000) (lisp-value < 35000 60000))proc:(primitive #<procedure:<>)
args(40000 60000)

(and (salary (Bitdiddle Ben) 60000) (salary (Hacker Alyssa P) 40000) (lisp-value < 40000 60000))proc:(primitive #<procedure:<>)
args(60000 60000)


c. 計算機部門にいない人が監督している人すべてと, その監督者の名前と担当.
(and
  (supervisor ?person ?supervisor)
  (job ?supervisor ?job)
  (not (job ?supervisor (computer . ?rest))))


;;; Query results:
(and (supervisor (Aull DeWitt) (Warbucks Oliver)) (job (Warbucks Oliver) (administration big wheel)) (not (job (Warbucks Oliver) (computer . ?rest))))
(and (supervisor (Cratchet Robert) (Scrooge Eben)) (job (Scrooge Eben) (accounting chief accountant)) (not (job (Scrooge Eben) (computer . ?rest))))
(and (supervisor (Scrooge Eben) (Warbucks Oliver)) (job (Warbucks Oliver) (administration big wheel)) (not (job (Warbucks Oliver) (computer . ?rest))))
(and (supervisor (Bitdiddle Ben) (Warbucks Oliver)) (job (Warbucks Oliver) (administration big wheel)) (not (job (Warbucks Oliver) (computer . ?rest))))
|#



