```
(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))
```

a. 金曜の朝, Benはその日にあるすべての会合について, データベースに質問したい. 彼はどういう質問を使うべきか.

```
;;; Query input:
(meeting ?title (Friday ?hour))

;;; Query results:
(meeting administration (Friday 1pm))
```

b. Alyssa P. Hackerは驚かなかった. 彼女は自分の名前を指定し, 自分の会合が聞けた方が有用と考えた. そこで彼女はある人の会合はwhole-companyの会合すべてと, その人の部門の会合のすべてを含むとする規則を設計した. Alyssaの規則の本体を補え.

```
(assert! (rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?type))
               (meeting ?division ?day-and-time)))))
```

c. Alyssaは水曜の朝, 仕事場に着き, その日どの会合に出なければならないか考えた. 上の規則を定義したとして, これを見つけるのに彼女はどういう質問をすべきか.

```
;;; Query input:
(meeting-time (Hacker Alyssa P) (Wednesday ?hour)
)
;;; Query results:
(meeting-time (Hacker Alyssa P) (Wednesday 4pm))
(meeting-time (Hacker Alyssa P) (Wednesday 3pm))
```