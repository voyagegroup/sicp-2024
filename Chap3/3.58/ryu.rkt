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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (display-stream-n s n)
  (define (iter index)
    (if (= index n)
        'done
        (begin
          (display (stream-ref s index))
          (display " ")
          (iter (+ index 1)))))
  (iter 0))

; 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den) ; quotientは二つの整数の, 整数の商を返す基本関数である. (quotient 10 5) → 2
   (expand (remainder (* num radix) den) den radix)))

(define q1 (expand 1 7 10))
(display-stream-n q1 20)
; 1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4 done
; 1/7の小数を表している
; 0.14285...


(quotient (* 1 10) 7) ; 1
(remainder (* 1 10) 7) ; 3（次のnum）


(define q2 (expand 3 8 10))
(display-stream-n q2 10)
; 3 7 5 0 0 0 0 0 0 0 done
; 3/8の小数を表している
; 0.375
; 有限小数であるため、あまりが0になって、そこから先は0になっている
