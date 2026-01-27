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


; --- 3.81


; 3.6
(define (rand-update x)
  (remainder (+ (* 1234 x) 567) 89))

(define rand
  (let ((x 1))
    (define (dispatch message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))
            (else (error "Unknown request -- RAND" message))))
    dispatch))


(define random-init 1)

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))


(define (random-generator request-stream)
 
  (define (process-requests requests current-seed)
    (if (stream-null? requests)
        the-empty-stream
        (let ((request (stream-car requests)))
          (cond ((eq? request 'generate) ; 生成処理
                 (let ((new-random (rand-update current-seed)))
                   (cons-stream new-random
                                (process-requests (stream-cdr requests)
                                                  new-random))))
                ((and (pair? request) (eq? (car request) 'reset)) ; リセット: (reset 100) のような形式を想定
                 (process-requests (stream-cdr requests) (cadr request)))))))
  (process-requests request-stream random-init))

; --- 3.82

#|
3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(random-in-range 25 30)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (50-percen)
  (< (random 2) 1)) 

(monte-carlo 1000 50-percen)
; 257/500
; だいたい0.5になっている

; 実装ここから

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials experiment) ; monte-carloの結果に四角形の面積をかける
     (* (- x2 x1) (- y2 y1))))

(define (unit-circle-test x y)
  (<= (+ (* x x) (* y y)) 1))


(define (estimate-pi-integral trials)
  (estimate-integral unit-circle-test -1.0 1.0 -1.0 1.0 trials))

(estimate-pi-integral 1000)
; 1回目: 3.184
; 2回目: 3.12
; 3回目: 3.16


流れ
1. 試行回数を決める
2. 試行回数分ランダムな点(x, y)をプロット
3. 各点が条件を満たすかをチェック
4. 満たした回数 / 総回数 = 確率
5. 確率 * 長方形の面積 = 積分値
|#

#|
3.5では、
(define (estimate-integral p x1 x2 y1 y2 trials))
で、trialsの数だけ試行回数を回していた。

これを、無限ストリームにするので、こんな感じになるはず
(define (estimate-integral p x1 x2 y1 y2))

3つのストリームが必要そう

1. 実験をするストリーム
プロットをして、それがtrueかfalseかのストリーム
experiment-stream → (true false true)

2. 確率のストリーム
プロットした結果、現状の確率
probability-stream = (1.0 0.5 0.66)

3. 積分値のストリーム
面積にする
確率 * 4
integral-stream = (4.0 2.0 2.66)
|#

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (unit-circle-test x y)
  (<= (+ (* x x) (* y y)) 1))

; 本文コピペ
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral p x1 x2 y1 y2)
  ; 初期化
  (define experiment-stream #f)
  (define probability-stream #f)
  (define integral-stream #f)
  
  ; 1. 実験ストリーム
  (set! experiment-stream
        (stream-map (lambda (_)
                      (p (random-in-range x1 x2)
                         (random-in-range y1 y2)))
                    integers))

  ; (display-stream-n experiment-stream 5)
  ; #f #f #f #t #t 
  
  ; 2. 確率ストリーム  
  (set! probability-stream
        (monte-carlo experiment-stream 0 0))
  ; (display-stream-n probability-stream 5)
  ; 0 0 0 1/4 2/5

  
  ; 3. 積分ストリーム
  (set! integral-stream
        (stream-map (lambda (prob)
                      (* prob (* (- x2 x1) (- y2 y1))))
                    probability-stream))
  ; (display-stream-n integral-stream 5)
  ; 0 0 0 1.0 1.6
 
  integral-stream)
  
  
(define x (estimate-integral unit-circle-test -1.0 1.0 -1.0 1.0))
(display-stream-n x 100)
; 4.0 2.0 2.6666666666666665 3.0 3.2 3.3333333333333335 3.4285714285714284 3.0 3.111111111111111 3.2 3.272727272727273 3.3333333333333335 3.3846153846153846 3.4285714285714284 3.466666666666667 3.5 3.2941176470588234 3.3333333333333335 3.3684210526315788 3.2 3.0476190476190474 2.909090909090909 2.9565217391304346 3.0 3.04 2.923076923076923 2.962962962962963 2.857142857142857 2.896551724137931 2.933333333333333 2.967741935483871 3.0 3.0303030303030303 3.0588235294117645 3.085714285714286 3.111111111111111 3.135135135135135 3.1578947368421053 3.1794871794871793 3.2 3.2195121951219514 3.238095238095238 3.255813953488372 3.1818181818181817 3.111111111111111 3.130434782608696 3.0638297872340425 3.0833333333333335 3.1020408163265305 3.12 3.1372549019607843 3.1538461538461537 3.169811320754717 3.185185185185185 3.2 3.2142857142857144 3.2280701754385963 3.2413793103448274 3.1864406779661016 3.2 3.2131147540983607 3.161290322580645 3.1746031746031744 3.1875 3.2 3.1515151515151514 3.1044776119402986 3.1176470588235294 3.130434782608696 3.142857142857143 3.1549295774647885 3.1666666666666665 3.1232876712328768 3.135135135135135 3.1466666666666665 3.1578947368421053 3.116883116883117 3.128205128205128 3.1392405063291138 3.15 3.1604938271604937 3.1707317073170733 3.180722891566265 3.142857142857143 3.1058823529411765 3.0697674418604652 3.0804597701149423 3.090909090909091 3.056179775280899 3.066666666666667 3.076923076923077 3.0869565217391304 3.096774193548387 3.106382978723404 3.1157894736842104 3.125 3.134020618556701 3.142857142857143 3.1515151515151514 3.12

