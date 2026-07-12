disjoinとstream-flatmapはどちらも複数のストリームを扱う関数である。
もし片方のストリームが無限であっても、他のストリームの全ての要素を全て出すことができるためストリームを交互に出している。
単純に連接してしまうと一つのストリームが無限であると他のストリームに到達することができない。

```scheme

(assert! (rule (same ?x ?x))) 
(assert! (rule (endless ?x) 
            (or (same ?x a) 
            (endless ?x)))) ;無限


(assert! (person alice)) 
(assert! (person bob))

(or (endless ?x) ;endlessの探索が無限に続く
        (person ?x))

(and 
    (person ?person) 
    (endless ?x)) ; aliceについての探索が無限に続き、bobに至らない
```
