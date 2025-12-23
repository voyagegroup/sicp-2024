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

; --- 3.59

(define (integrate-series stream)
  (stream-map / stream integers))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

; --- 3.60

(define (mul-series s1 s2)
  (cons-stream 
   (* (stream-car s1) (stream-car s2))
   (add-streams 
    (scale-stream (stream-cdr s2) (stream-car s1))
    (mul-series (stream-cdr s1) s2))))

; --- 3.61

(define (invert-unit-series s)
  (cons-stream 
   1 ; 定数項は1
   (scale-stream; 符号を反転(-1倍)
    (mul-series 
     (stream-cdr s) ; S_R(sの高次項)
     (invert-unit-series s))  ; X(自己参照)
    -1)))

; 3.62

; A / B = A * (1/B)
; (1/B) → 3.61でやったやつ
; 定数項は1固定だったので、定数項で割る
; （e.g. 2+3x+5x^2 とかだったら2で割る）

(define (div-series num denom)
  (let ((c (stream-car denom)))
    (if (= c 0)
        (error "0の定数項です")
        (let ((new-denom
               (scale-stream denom (/ 1 c))))
          (mul-series
           num
           (scale-stream
            (invert-unit-series new-denom)
            (/ 1 c)))))))

; テスト: (1+x) ÷ (1-x)
(display "テスト1: (1+x) ÷ (1-x)\n")
(define one-plus-x 
  (cons-stream 1 (cons-stream 1 (cons-stream 0 one-plus-x))))
(define one-minus-x 
  (cons-stream 1 (cons-stream -1 (cons-stream 0 one-minus-x))))

(define quotient1 (div-series one-plus-x one-minus-x))
(display "(1+x)/(1-x) = ")
(display-stream-n quotient1 8)
(display "\n期待値: 1 + 2x + 2x2 + 2x3 + ...\n\n")

; tan x = sin x / cos x
(define tangent-series
  (div-series sine-series cosine-series))