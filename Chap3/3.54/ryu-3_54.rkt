#lang sicp
; --- 3.5.1

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; (stream-car (cons-stream x y)) = x
(define (stream-car stream) (car stream))
; (stream-cdr (cons-stream x y)) = y
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; --- 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; --- 3.5.2

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))


(define integers (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define twos (cons-stream 2 twos))
(define mul-test (cons-stream 1 (mul-streams twos mul-test)))
(stream-ref mul-test 0) ; 1
(stream-ref mul-test 1) ; 2
(stream-ref mul-test 2) ; 4
(stream-ref mul-test 3) ; 8

; n番目がn+1になる
(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers)))) ; integersは2からスタートさせたかったので、cdrを取った
; (n + 1)!
(stream-ref factorials 0) ; (0 + 1)! = 1
(stream-ref factorials 1) ; (1 + 1)! = 2! = 2
(stream-ref factorials 2) ; (2 + 1)! = 3! = 6
(stream-ref factorials 3) ; (3 + 1)! = 4! = 24
(stream-ref factorials 4) ; 5! = 120

