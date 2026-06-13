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
```

「SがFの息子であり, かつFがGの息子であるなら, SはGの孫[grandson]である」

```
(assert! (rule (grandson ?grandpa ?grandson)
      (and (son ?father ?grandson)
           (son ?grandpa ?father))))
```

```
;;; Query input:
(grandson Adam ?s)

;;; Query results:
(grandson Adam Enoch)
```

「WがMの妻であり, かつSがWの息子であるなら, SはMの息子である」

```
(assert! (rule (nuclear-family ?husband ?son)
      (and (wife ?husband ?wife)
           (son ?wife ?son))))
```

```
;;; Query input:
(nuclear-family ?m ?s
)
;;; Query results:
(nuclear-family Lamech Jubal)
(nuclear-family Lamech Jabal)
```