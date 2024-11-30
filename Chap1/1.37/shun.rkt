#lang racket

; a
(define (cont-frac n d k)
  (define (hoge i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (hoge (+ i 1))))))
  (hoge 1))


(cont-frac (lambda (i) 1.0)
          (lambda (i) 1.0)
          12)
; 0.6180257510729613

; b

(define (cont-frac n d k)
  (define (hoge i result)
    (if (= i 0)
        result
        (hoge (- i 1) (/ (n i) (+ (d i) result)))))
  (hoge (- k 1) (/ (n k) (d k))))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

; 0.6180555555555556