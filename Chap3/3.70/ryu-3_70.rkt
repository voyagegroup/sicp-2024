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


; --- 3.6.3
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

; 「重み関数」W(i, j)を定義し, W(i1, j1) < W(i2, j2)なら, (i1, j1)は(i2, j2)より小さいと規定することである.
; e.g. W(i, j) = i + j
; (1, 1) → 重み2
; (1, 2) → 重み3
; (1, 3) → 重み4

; merge-weightedはもう一つの引数weightを取る他は, mergeに似た手続きmerge-weightedを書け
; weight は対の重みを計算する手続きで, 要素が結果の混ぜ合せたストリームに現れる順を決めるのに使う.
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

; a. i+j
(define (sum-weight pair)
  (+ (car pair) (cadr pair)))

(define i+j
  (weighted-pairs integers integers sum-weight))
(display-stream-n i+j 20)
; (1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (1 6) (2 5) (3 4) (1 7) (2 6) (3 5) (4 4) (1 8) (2 7) (3 6) (4 5) done



; b. 2i + 3j + 5ij 
(define (2i+3j+5ij pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define (not-divisible? n)
  (and (not (= (remainder n 2) 0))
       (not (= (remainder n 3) 0))
       (not (= (remainder n 5) 0))))

(define filtered-stream
  (stream-filter (lambda (n) (not-divisible? n))
                 integers))

(define special-ordered-pairs
  (weighted-pairs filtered-stream filtered-stream 2i+3j+5ij))


(display-stream-n special-ordered-pairs 20)
; (1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47) (1 49) (1 53) (7 11) (1 59) (1 61) (7 13) done
; (1 1) → 2 + 2 + 5 = 9
; (1 7) → 1 + 21 + 35 = 57


