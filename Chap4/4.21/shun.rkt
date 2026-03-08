#lang sicp

; a
((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)


((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fib k)
      (if (<= k 1)
          k
          (+ (fib fib (- k 1)) (fib fib (- k 2)))))))
 4)

; b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 1)
; (ev? 1)→ (od? 0)→ false

(f 2)
; (ev? 2)→ (od? 1)→ (ev? 0)→ true

; evが奇数回呼び出されるならば、奇数である。偶数回呼び出されるならば、偶数である。