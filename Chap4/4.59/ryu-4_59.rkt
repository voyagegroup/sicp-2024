#lang sicp

; a. 金曜の朝, Benはその日にあるすべての会合について, データベースに質問したい. 彼はどういう質問を使うべきか.
(meeting ?division (Friday ?time))

; b. Alyssa P. Hackerは驚かなかった. 彼女は自分の名前を指定し, 自分の会合が聞けた方が有用と考えた. そこで彼女はある人の会合はwhole-companyの会合すべてと, その人の部門の会合のすべてを含むとする規則を設計した. Alyssaの規則の本体を補え.
(rule (meeting-time ?person ?day-and-time)
      ⟨rule-body⟩)


; こんな感じなゴール
(meeting-time (Hacker Alyssa P) ?day-and-time)

; こんなイメージ
(or
 whole-company の会合
 その人の部門の会合)

; job一覧
(job ?person (?division . ?rest))

(assert!
 (rule (meeting-time ?person ?day-and-time)
       (or
        (meeting whole-company ?day-and-time)
        (and
         (job ?person (?division . ?rest))
         (meeting ?division ?day-and-time)))))


;;; Query input:

(meeting-time (Hacker Alyssa P) ?day-and-time)

;;; Query results:
(meeting-time (Hacker Alyssa P) (Wednesday 4pm))
(meeting-time (Hacker Alyssa P) (Wednesday 3pm))

; c. Alyssaは水曜の朝, 仕事場に着き, その日どの会合に出なければならないか考えた. 上の規則を定義したとして, これを見つけるのに彼女はどういう質問をすべきか.
;;; Query input:

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

;;; Query results:
(meeting-time (Hacker Alyssa P) (Wednesday 4pm))
(meeting-time (Hacker Alyssa P) (Wednesday 3pm))
