#lang sicp

#|
問題 4.57

person-1がperson-2と同じ担当であるか. person-1の仕事をする誰かがperson-2の仕事が出来, しかもperson-1がperson-2と同じ人でなければ, person-1はperson-2に代る[replace]ことが出来るとする規則を定義せよ. その規則を使い, 次のことを見つける質問を作れ:

a. Cy D. Fectに代れる人すべて;

b. 誰かに代れて, その誰かの方が多くの給料を貰っている人すべてと, 両者の給料.
|#

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

; person-1 と person-2 が同じ担当である
(job ?person-1 ?job)
(job ?person-2 ?job)

; person-1 の仕事をする誰かが person-2 の仕事ができる
(can-do-job ?job-1 ?job-2)

; 同じ人ではない
(not (same ?person-1 ?person-2))

(assert!
 (rule (replace ?person-1 ?person-2)
       (and
        (or
         (and (job ?person-1 ?job)
              (job ?person-2 ?job))
         (and (job ?person-1 ?job-1)
              (job ?person-2 ?job-2)
              (can-do-job ?job-1 ?job-2)))
        (not (same ?person-1 ?person-2)))))


; a.

;;; Query input:
(replace ?person (Fect Cy D))

;;; Query results:
(replace (Bitdiddle Ben) (Fect Cy D))
(replace (Hacker Alyssa P) (Fect Cy D))


; b.

;;; Query input:
(and
 (replace ?person-1 ?person-2)
 (salary ?person-1 ?salary-1)
 (salary ?person-2 ?salary-2)
 (lisp-value < ?salary-1 ?salary-2))

;;; Query results:proc:(primitive #<procedure:<>)
args(25000 150000)

(and (replace (Aull DeWitt) (Warbucks Oliver)) (salary (Aull DeWitt) 25000) (salary (Warbucks Oliver) 150000) (lisp-value < 25000 150000))proc:(primitive #<procedure:<>)
args(35000 30000)
proc:(primitive #<procedure:<>)
args(40000 30000)
proc:(primitive #<procedure:<>)
args(60000 25000)
proc:(primitive #<procedure:<>)
args(60000 35000)
proc:(primitive #<procedure:<>)
args(60000 40000)
proc:(primitive #<procedure:<>)
args(40000 35000)
proc:(primitive #<procedure:<>)
args(35000 40000)

(and (replace (Fect Cy D) (Hacker Alyssa P)) (salary (Fect Cy D) 35000) (salary (Hacker Alyssa P) 40000) (lisp-value < 35000 40000))