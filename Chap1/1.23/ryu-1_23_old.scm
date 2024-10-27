(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))


(prime? 11)
(= 11 (smallest-divisor 11))
(find-divisor 11 2)
(cond ((> (square 2) 11) 11)
      ((divides? 2 11) 2)
      (else (find-divisor 11 (+ 2 1))))
; (> 4 11) -> #f
; (divides? 2 11) -> #f
(find-divisor 11 3)
(find-divisor 11 4)
; -> 11


