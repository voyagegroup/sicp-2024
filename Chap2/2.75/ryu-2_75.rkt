#lang sicp

; --- 参考のreal-imag

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (display op)
    (newline)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define z (make-from-real-imag 3 4))
(z 'real-part) ; 3
(z 'imag-part) ; 4
(z 'magnitude) ; 5
(z 'angle)     ; 0.9272952180016122

; --- ここから回答

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


(define z2 (make-from-mag-ang 5 (atan 4 3)))
(z2 'real-part) ; 3.0
(z2 'imag-part) ; 3.9999999999999996
(z2 'magnitude) ; 5
(z2 'angle)     ; 0.9272952180016122
