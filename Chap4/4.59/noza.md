# 問題 4.59

会議スケジュールのデータベースとルールを扱う。

## データベースのアサーション

```scheme
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))
```

---

## a. 金曜日に会議がある部門

```scheme
(meeting ?division (Friday ?time))
```

---

## b. `meeting-time` ルールの定義

個人が属する部門の会議、または全社会議に参加する。

```scheme
(rule (meeting-time ?person ?day-and-time)
      (or (and (job ?person (?division . ?type))
               (meeting ?division ?day-and-time))
          (meeting whole-company ?day-and-time)))
```

---

## c. Alyssa P. Hacker の水曜日の会議

```scheme
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
```
