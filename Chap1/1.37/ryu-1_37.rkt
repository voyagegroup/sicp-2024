#lang sicp

; φ ≒ 1.6180
(/ 1.0 1.6180 )
; -> 0.6180469715698392
; 1 / φ ≒ 0.6180469715698392

; 再帰的プロセス
(define (cont-frac n d k)
  (define (cont-iter i)
    (if (< k i)
        0
        (/ (n i) (+ (d i) (cont-iter (+ i 1)))))) ; 
  (cont-iter 1))

; memo (lamda (i) 1.0) の挙動
; (define (a x)
;   (x 3))
; (a (lambda (i) 1.0))
; -> 1.0
; i の値にかかわらず 1.0 になる

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

; (cont-iter 1)
; (if (< 100 1)
;     0
;     (/ ((lambda (i) 1.0) 1) (+ ((lambda (i) 1.0) 1) (cont-iter 2))))

; -> 0.6180339887498948

; Q. 4桁の精度の近似を得るのに, kはどのくらい大きくしなければならないか.
; 1からkまでの結果を出力する手続き
(define (test k)
  (define (test-iter count)
    (if (= count k)
        (display "end\n")
        (begin
          (display "k: ")
          (display count)
          (newline)
          (display (cont-frac (lambda (i) 1.0)
                              (lambda (i) 1.0)
                              count))
          (newline)
          (test-iter (+ count 1)))))
  (test-iter 1))

(test 15)
; ----
; k: 1
; 1.0
; k: 2
; 0.5
; k: 3
; 0.6666666666666666
; k: 4
; 0.6000000000000001
; k: 5
; 0.625
; k: 6
; 0.6153846153846154
; k: 7
; 0.6190476190476191
; k: 8
; 0.6176470588235294
; k: 9
; 0.6181818181818182
; k: 10
; 0.6179775280898876
; k: 11
; 0.6180555555555556
; k: 12
; 0.6180257510729613
; k: 13
; 0.6180371352785146
; k: 14
; 0.6180327868852459
; end
; --

; 0.6180 になっているのは k=11 から

; 反復的プロセス
(define (cont-frac-2 n d k)
  (define (cont-iter i result)
    (if (= 0 i)
        result
    (cont-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-iter k 0))

(cont-frac-2 (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)
    
; -> 0.6180339887498948    
