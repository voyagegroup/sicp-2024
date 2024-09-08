(define (largest-to-squares-sum x y z)
  (cond ((and (<= x y) (<= x z)) (+ (* y y) (* z z)))
        ((and (<= y x) (<= y z)) (+ (* x x) (* z z)))
        (else (+ (* x x) (* y y)))))
        
(largest-to-squares-sum 1 2 3)
; 13

(largest-to-squares-sum 5 2 2)
; 29

(largest-to-squares-sum 2 2 5)
; 29