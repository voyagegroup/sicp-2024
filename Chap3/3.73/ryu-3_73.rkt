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


; --- 3.5.3
; interleaveは二つのストリームから要素を交互にとるので, 第一のストリームが無限であっても, 第二のストリームのすべての要素は, いつかは混ぜ合されたストリームへ行く道を見つける.
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


; --- 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


; --- 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((w1 (weight s1car)) ; 追加
                 (w2 (weight s2car)))
             (cond ((< w1 w2)
                    (cons-stream s1car 
                                 (merge-weighted (stream-cdr s1) s2 weight)))
                   ((> w1 w2)
                    (cons-stream s2car 
                                 (merge-weighted s1 (stream-cdr s2) weight)))
                   (else  ; 同じ重みの場合、両方含める。 e.g. (1, 3), (2, 2)
                    (cons-stream s1car
                                 (cons-stream s2car
                                              (merge-weighted (stream-cdr s1)
                                                              (stream-cdr s2)
                                                              weight))))))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))


; --- 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i-stream v0)
    (define v
      (add-streams
       (scale-stream i-stream  R) ; scale: R
       (integral (scale-stream i-stream (/ 1 C)) ; scale: I/C → integral
                 v0 ; 初期値
                 dt)))
    v))


(define RC1 (RC 5 1 0.5))
; 定電流1Aの入力
(define constant-current (cons-stream 1.0 constant-current))

; 初期電圧0Vでシミュレーション
(define voltage-response (RC1 constant-current 0))



(display-stream-n voltage-response 10)
; → 5.0 5.5 6.0 6.5 7.0 7.5 8.0 8.5 9.0 9.5 done







  