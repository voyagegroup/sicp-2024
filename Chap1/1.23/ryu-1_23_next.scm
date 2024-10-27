(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

; 入力が2なら3 を返し, そうでなければ入力に2足したものを返す手続きnext
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (prime? n)
  (= n (smallest-divisor n)))


(prime? 11)
(= 11 (smallest-divisor 11))
(find-divisor 11 2)
(cond ((> (square 2) 11) 11)
      ((divides? 2 11) 2)
      (else (find-divisor 11 (next 2))))
; (> 4 11) -> #f
; (divides? 2 11) -> #f
(find-divisor 11 (next 2))
(find-divisor 11 (if (= 2 2)
                  3
                  (+ 2 2)))
; (= 2 2) -> #t
(find-divisor 11 3)
(find-divisor 11 (next 3))
(find-divisor 11 (= 3 2)
              3
              (+ 3 2))
; (= 3 2) -> #f
(find-divisor 11 5)
; -> 11
