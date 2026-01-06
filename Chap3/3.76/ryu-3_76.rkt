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

; --- 3.74
(define (sign-change-detector current previous)
  (cond ((and (< previous 0) (>= current 0)) 1) ; マイナスからプラス
        ((and (>= previous 0) (< current 0)) -1) ; プラスからマイナス
        (else 0)))

(define (list->stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (list->stream (cdr lst)))))

(define sense-data 
  (list->stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))


; --- 3.76


#| 部品化まえ
(define (make-zero-crossings input-stream last-value last-avg)
  (if (stream-null? input-stream)
      the-empty-stream
      (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream (sign-change-detector avpt last-avg)
                     (make-zero-crossings (stream-cdr input-stream)
                                          (stream-car input-stream)
                                          avpt)))))
|#

(define (average x y) (/ (+ x y) 2))
(define (smooth stream)
  (stream-map average
              stream
              (stream-cdr stream)))

; 平滑化なしのゼロ交差検出
(define (zero-crossings-raw stream)
  (stream-map sign-change-detector
              stream
              (cons-stream 0 stream)))

; 平滑化ありのゼロ交差検出
(define (zero-crossings-smoothed stream)
  (zero-crossings-raw (smooth stream)))

(define noisy-signal 
  (list->stream '(1 1.2 0.9 1.1 -0.1 0.1 -0.2 -2 -1.8 -2.1 0.1 0.2 3)))

(display "平滑化した信号: ")
(display-stream-n (smooth noisy-signal) 12)

(display "平滑化なしのゼロ交差: ")
(display-stream-n (zero-crossings-raw noisy-signal) 13)

(display "平滑化ありのゼロ交差: ")
(display-stream-n (zero-crossings-smoothed noisy-signal) 12)


#|
平滑化した信号: 1.1 1.05 1.0 0.5 0.0 -0.05 -1.1 -1.9 -1.9500000000000002 -1.0 0.15000000000000002 1.6 done
平滑化なしのゼロ交差: 0 0 0 0 -1 1 -1 0 0 0 1 0 0 done
平滑化ありのゼロ交差: 0 0 0 0 0 -1 0 0 0 0 1 0 done
|#





















