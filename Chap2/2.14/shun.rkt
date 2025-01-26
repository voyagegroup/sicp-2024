#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* c (/ p 100)) 2)))
    (make-center-width c w)))

(define (percent i)
  (* (/ (- (upper-bound i) (lower-bound i)) (center i)) 100))

(define a
  (make-center-percent 100 1))

(define b
  (make-center-percent 200 1))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; 設問通り、a/aとa/bを計算する。
(define daa
  (div-interval a a))

(define dab
  (div-interval a b))

(percent daa); 1.9999500012499796
(newline)
(percent dab); 1.9999500012499796

; 誤差が共に2倍になっていることがわかる

; par1でabの計算を行う時、それぞれを計算すると

(define mab
  (mul-interval a b))

(define aab
  (add-interval a b))

(percent mab) ; 1.999950001249969
(percent aab); 1.0

; 掛け算では誤差が足されてほぼ2になるが、足し算では変化しない。
; この結果を割った時の誤差は

(define d1
  (div-interval mab aab))

(percent d1) ; 2.999800014998875

; 誤差が足されてほぼ3になる。

; 他方、par2においては、

(define one (make-center-percent 1 0))

(define doa
  (div-interval one a))

(define dob
  (div-interval one b))

(percent doa) ; 1.0000000000000036
(percent dob) ; 1.0000000000000036

; 誤差が足されて、ほぼ1になる。

(define a2
  (add-interval doa dob))

(percent a2) ; 0.9999999999999978

; 誤差はほぼ変わらず、ほぼ1のままである。

(define d2
  (div-interval one a2))

(percent d2) ; 0.9999999999999858

; 誤差はほぼ変わらず、1のままである。

; この誤差の違いが、二つの計算結果の違いである。
