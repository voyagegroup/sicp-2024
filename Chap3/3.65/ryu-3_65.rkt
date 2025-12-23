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

; --- 3.55

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) 
                            (partial-sums s))))

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

; --- 3.64
(define (stream-limit stream t)
  (let ((car (stream-car stream))
        (cdr (stream-car (stream-cdr stream))))
    ; (display car)
    ; (display cdr)

    (if (< (abs (- car cdr)) t)
        cdr ; 誤差よりも小さくなったので返す
        (stream-limit (stream-cdr stream) t))))

; 3.65

; log 2 = 0.69314 71805 59945 30941 72321...

; piのやつの動作をみてみる
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))


(display-stream-n (pi-summands 1) 10)
; 1.0 -0.3333333333333333 0.2 -0.14285714285714285 0.1111111111111111 -0.09090909090909091 0.07692307692307693 -0.06666666666666667 0.058823529411764705 -0.05263157894736842 done
; 1, -1/3, 1/5, -1/7...になっている

; ln2のsummandsをつくる
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(display-stream-n (ln2-summands 1) 10)
; 1.0 -0.5 0.3333333333333333 -0.25 0.2 -0.16666666666666666 0.14285714285714285 -0.125 0.1111111111111111 -0.1 done
; 1, -1/2, 1/3, -1/4...になっている

; たす
(define ln2-stream
  (partial-sums (ln2-summands 1)))
(display-stream-n ln2-stream 10)
; 1.0 0.5 0.8333333333333333 0.5833333333333333 0.7833333333333332 0.6166666666666666 0.7595238095238095 0.6345238095238095 0.7456349206349207 0.6456349206349207 done
; 10回やったら近似ができてきている

; (display (stream-limit ln2-stream 0.00001))
; めっちゃ時間がかかる（1分くらい待ってやめた）

; Euler加速を使う
(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define ln2-euler
  (euler-transform ln2-stream))
(display-stream-n ln2-euler 10)

(display (stream-limit ln2-euler 0.00001))
; 0.6931515803143488
; 一瞬ででた

; tableauを使う


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define ln2-tableau
  (accelerated-sequence euler-transform ln2-stream))
(display-stream-n ln2-tableau 10)
(display (stream-limit ln2-tableau 0.00001))
; 0.6931471960735491
; こっちも一瞬ででた