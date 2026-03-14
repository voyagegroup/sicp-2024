#lang sicp

; 問題 4.21 a
(define factorial-10
  ((lambda (n)
     ((lambda (fact)
        (fact fact n))
      (lambda (ft k)
        (if (= k 1)
            1
            (* k (ft ft (- k 1)))))))
   10))

(define fibonacci-10
  ((lambda (n)
     ((lambda (fib)
        (fib fib n))
      (lambda (fb k)
        (if (< k 2)
            k
            (+ (fb fb (- k 1))
               (fb fb (- k 2)))))))
   10))

; 問題 4.21 b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))

; 期待値: (3628800 55 true false false true true)
(list factorial-10
      fibonacci-10
      (f 0)
      (f 1)
      (f 5)
      (f 6)
      (f 10))
