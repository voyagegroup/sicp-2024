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
#|
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
|#

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

; --- 3.5.4

#|
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)
|#

; --- 3.77
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand))) ; 追加
                    (if (stream-null? integrand)
                        the-empty-stream
                        (integral (delay (stream-cdr integrand))
                                  (+ (* dt (stream-car integrand))
                                     initial-value)
                                  dt)))))

(define (solve f y0 dt)
  ; まず空のストリームから始める
  (define y #f)
  (define dy #f)
  ; yを定義
  (set! y (integral (delay (or dy the-empty-stream)) y0 dt))
  ; dyを定義  
  (set! dy (stream-map f y))
  ; yを更新(正しいdyで)
  (set! y (integral (delay dy) y0 dt))
  y)

; (stream-ref (solve (lambda (y) y) 1 0.001) 1000)

; --- 3.78

#|
(define (solve-2nd a b dt y0 dy0)
  ; 空をつくる
  (define y #f)
  (define dy #f)  
  (define ddy #f)
  
  ; dy を定義: ddy を積分、初期値 dy0
  (set! dy (integral (delay ddy) dy0 dt))
  
  ; y を定義: dy を積分、初期値 y0  
  (set! y (integral (delay dy) y0 dt))
  
  ; ddy を定義: a*dy + b*y
  (set! ddy (add-streams (scale-stream dy a)
                         (scale-stream y b)))
  
  y)
|#

; --- 3.79
; (a b ..) → (f ...) へ変更
(define (solve-2nd f dt y0 dy0)
  ; 空をつくる
  (define y #f)
  (define dy #f)  
  (define ddy #f)
  
  ; dy を定義: ddy を積分、初期値 dy0
  (set! dy (integral (delay ddy) dy0 dt))
  
  ; y を定義: dy を積分、初期値 y0  
  (set! y (integral (delay dy) y0 dt))
  
  ; ddy を定義: f(dy, y)
  ; stream-mapを使う
  (set! ddy (stream-map f dy y))
  
  y)

; --- 3.80
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC #f)
    (define iL #f)
    (define dvC #f)
    (define diL #f)

    (set! vC (integral (delay dvC) vC0 dt))
    (set! iL (integral (delay diL) iL0 dt))
    (set! dvC (scale-stream iL (/ -1 C)))
    (set! diL (add-streams (scale-stream vC (/ 1 L))
                           (scale-stream iL (- (/ R L)))))
    
    (cons vC iL)))  ; 両方のストリームを返す


; R=1Ω, C=0.2F, L=1H, dt=0.1s, iL0=0A, vC0=10V
(define rlc1 (RLC 1 1 0.2 0.1))
(define result (rlc1 10 0))
(define vC-stream (car result))
(define iL-stream (cdr result))


(display "vC(0) = ") (display (stream-ref vC-stream 0)) (newline)
(display "iL(0) = ") (display (stream-ref iL-stream 0)) (newline)
(display "vC(1s) = ") (display (stream-ref vC-stream 10)) (newline)
(display "iL(1s) = ") (display (stream-ref iL-stream 10)) (newline)