> 10

10

> (+ 5 3 4)  

12

> (- 9 1)  

8

> (/ 6 2)  

3

> (+ (* 2 4) (- 4 6))  

8 - 2
= 6

> (define a 3)  
> (define b (+ a 1))  
> (+ a b (* a b))  
> (= a b)  

(+ 3 4 (* 3 4))
= (+ 3 4 12)
= 19

> (if (and (> b a) (< b (* a b)))  b  a) 

(if (and (> b a) (< b ab))  b  a) 
=(if (and (> b a) (< b ab))  b  a) 
=(if (and (> 4 3) (< b ab))  b  a)
= 4

> (cond ((= a 4) 6)  ((= b 4) (+ 6 7 a))  (else 25))  

(cond ((= a 4) 6)  ((= 4 4) 16)  (else 25))
=16

> (+ 2 (if (> b a) b a))  

=6

> (* (cond ((> a b) a)  ((< a b) b)  (else -1))  (+ a 1)) 

=(* (cond ((> a b) a)  ((< a b) b)  (else -1))  (+ a 1))
=(* b  (+ a 1))
=(* b  4)
=16