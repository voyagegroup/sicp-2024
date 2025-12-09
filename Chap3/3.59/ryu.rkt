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


; 3.59

; A
(define (integrate-series stream)
  (stream-map / stream integers))

(define ones (cons-stream 1 ones)) ; f(x) = 1
(display-stream-n (integrate-series ones) 5)
; → 1 1/2 1/3 1/4 1/5

(display-stream-n (integrate-series integers) 5) ;f(x) = 1 + 2x + 3x^3
; → 1 1 1 1 1
; 1/1, 2/2 ....

; B
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(display-stream-n exp-series 5)
; 1 1 1/2 1/6 1/24 done

;  正弦の微分が余弦であり, 余弦の微分が正弦の符号を変えたものであるという事実から出発し, 正弦と余弦の級数を生成する方法を示せ.

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1)))) ; scale-streamを使って符号をかえる

(display-stream-n sine-series 5)
; 0 1 0 -1/6 0 
(display-stream-n cosine-series 5)
; 1 0 -1/2 0 1/24