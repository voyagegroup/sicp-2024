;
#lang sicp

(define (f-recu n)
  (cond ((< n 3) n)
        (else (+ (f-recu (- n 1))
                 (* 2 (f-recu (- n 2)))
                 (* 3 (f-recu (- n 3)))))))
(f-recu 1); 1
(f-recu 2); 2
(f-recu 5); 25
(f-recu 10); 1892

(define (f-iter n)
  (cond ((< n 3) n)
        (else (iter 3 0 1 2 n))))
(define (iter i f0 f1 f2 n)
  (cond ((> i n) f2)
        (else (iter (+ i 1)
                    f1
                    f2
                    (+ f2 (* 2 f1) (* 3 f0))
                    n))))

(f-iter 1); 1
(f-iter 2); 2
(f-iter 5); 25
(f-iter 10); 1892
