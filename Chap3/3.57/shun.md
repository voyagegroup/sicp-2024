```racket
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
```

> add-streams手続きに基づくfibsの定義を使い, n番目のFibonacci数を計算する時, 加算は何回実行されるか.

最初に定義された、0,1を除くn-2回実行される。

以下では3番目のfibonacci数を取り出すときにaddが一度実行されることを確認できる。
```racket
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(stream-ref fibs 3)

; (stream-ref (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs)) 2)

; (stream-ref (add-streams (stream-cdr fibs)
                                         fibs) 1)

; (stream-ref (stream-map + (stream-cdr fibs) fibs) 1)

; (stream-ref (stream-map + (force (cdr fibs)) fibs) 1)

; (stream-ref (stream-map + (force (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))) fibs) 1)

; (stream-ref (stream-map + (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs)) fibs) 1)

; (stream-ref (stream-cons
       (apply + (map stream-car (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs)) fibs))
       (apply stream-map
              (cons + (map stream-cdr (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs)) fibs)))) 1)

; (stream-ref (stream-cons
       (apply + '(1 0))
       (apply stream-map
              (cons + (map stream-cdr (cons + (map stream-cdr (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs)) fibs)))))) 1)

; (stream-ref (stream-cons
       1
       (apply stream-map
              (cons + (map stream-cdr (cons + (map stream-cdr (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs)) fibs)))))) 1)

; (stream-car (stream-cons
       1
       (apply stream-map
              (cons + (map stream-cdr (cons + (map stream-cdr (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs)) fibs)))))))

; 1
```

delayがメモ化を行わない場合、fibsが登場するたびに再度n-2回のaddが実行される。
結果として指数オーダーで増えていく。