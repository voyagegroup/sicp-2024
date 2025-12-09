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

; -- 3.57

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(display-stream-n fibs 10)


#|
# メモ化ありの場合

初期値
fibs[0]
fibs[1]

fibs[2] = fibs[0] + fibs[1] ; 1回加算
fibs[3] = fibs[1] + fibs[2] ; 1回加算
...
fibs[n] = fibs[n-1] + fibs[n-2] ; 1回加算

メモ化ありの場合は、nを計算するためのn-1とn-2がメモ化されているため、O(n)の計算量でできる。


# メモ化なしの場合

fibs[2] = fibs[0] + fibs[1] ; 1回加算
fibs[3] = fibs[1] + (fibs[0] + fibs[1]) ; 2回加算
fibs[4] = (fibs[0] + fibs[1]) + (fibs[1] + fibs[0] + fibs[1])
fibs[5] = fibs[1] + (fibs[0] + fibs[1]) +　(fibs[0] + fibs[1]) + (fibs[1] + fibs[0] + fibs[1])
...

メモ化していない場合、nを計算するためのn-1とn-2を毎回用意する必要があり、更にnが増えると、そのn-1とn-2を用意するためにさらにn-3を用意すると続いていくため、nが増えるたびに指数的に増えていく。
|#