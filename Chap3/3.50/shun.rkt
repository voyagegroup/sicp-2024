#lang sicp

(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b)
     (cons-stream a b)]))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


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

(define (stream-car stream) (car stream))


(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2 1))

(define (find-divisor n test-divisor count)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1) (+ count 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; --- ストリームを作るヘルパー ------------------------

;; 1からnまでのストリーム
(define (stream-enumerate n)
  (if (= n 0)
      the-empty-stream
      (cons-stream n (stream-enumerate (- n 1)))))

;; stream をリストに変換(表示用)
(define (stream->list s n)
  (if (= n 0)
      '()
      (cons (stream-car s)
            (stream->list (stream-cdr s) (- n 1)))))


;; --- テスト --------------------------------------------

(define s1 (stream-enumerate 5))  ; → (5 4 3 2 1)
(define s2 (stream-enumerate 5))  ; → (5 4 3 2 1)

(stream-map + s1 s2)

(stream->list s-sum 5)  ; => (10 8 6 4 2)