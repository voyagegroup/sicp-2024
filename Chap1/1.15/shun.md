
```scheme
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))
```

a. (sine 12.15)の評価で, 手続きpは何回作用させられたか.
5回

```scheme
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
```

b. (sine a)の評価で, 手続きsineの生成するプロセスが使うスペースとステップ数の増加の程度は(aの関数として)何か.

この評価は遅延乗算を行う。そのため置き換えモデルは膨張と収縮を行い、こうしたプロセスを再帰的プロセスという。
よってステップごとに新しくスペースを取っていくため、ステップ数とスペースの増加の程度は同じである。
ステップ数はaが0.1になるまでaに1/3をかけるため、ステップ数をnとすると
0.1=a/3^n
0.1*3^n=a
3^n=a/0.1
n=log3 a/0.1

**底と係数を省いて、log aが増加の程度である。**



aの途中式というかメモ
```scheme

(p (p (p (p (- (* 3 0.05) (* 4 (cube 0.05)))))))
(p (p (p (p (- (* 3 0.05) (* 4 (* 0.05 0.05 0.05)))))))
(p (p (p (p (- (* 3 0.05) (* 4 0.000125))))))
(p (p (p (p (- 0.15 0.0005)))))
(p (p (p (p 0.1495))))
(p (p (p (- (* 3 0.1495) (* 4 (cube 0.1495)))))

(p (p (p (p (p 0.05)))))
(p (p (p (p 0.1495))))

(define (sine 12.15)
   (if (not (> (abs 12.15) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(define (sine 12.15)
   (if (false) ;(> 12.15 0.1)はtrue。その否定なのでfalse。代替節を評価
       angle
       (p (sine (/ angle 3.0)))))

(define (sine 12.15)
   (if (false) 
       angle
       (p (sine 4.05))))

(define (sine 12.15)
   (if (false) 
       angle
       (p (if (false)
       angle
       (p (sine (/ 4.15 3.0)))))))

```