(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; gcd(206, 40)
; -> (if (= 40 0) 206 (gcd 40 (remainder 206 40)))
; -> (gcd 40 (remainder 206 40))
; -> (if (= (remainder 206 40) 0)
;      40
;      (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; -> (if (= (remainder 40 (remainder 206 40)) 0)
;      (remainder 206 40)
;      (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))); 2
; -> (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; -> (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0); 6
;      (remainder 40 (remainder 206 40))
;      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))); 6
; -> (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; -> (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0); 13
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;      (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
; -> (remainder (remainder 206 40) (remainder 40 (remainder 206 40))); 17
; -> 2
