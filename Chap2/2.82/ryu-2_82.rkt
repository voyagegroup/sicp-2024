#lang sicp
; (op typea1 type2 type3) のような場合という理解
; type1に変換できるか、type2に変換できるか、type3に変換できるか、っていうのが愚直な処理。
; 引数が増えれば増えるほど、現実的じゃなくなってくる。
; 思うのは、(add int int int) みたいなのをして。
; (add intVal stringVal doubleVal) として使いたいという場合は、addを使う側で(add intVal (string->int stringVal) (double->int doubleVal)) みたいに、使い手側が型を決めたほうがコンパイラ側も楽だよなって思う。

