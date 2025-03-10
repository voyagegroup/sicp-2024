#lang racket

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

; ここから

; 入力nに対して、3つの整数の組を生成する
(define (enumerate-interval-trio low high)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval (+ j 1) high)))
                      (enumerate-interval (+ i 1) high)))
           (enumerate-interval low high)))

; trioの和がsに等しいか判定
(define (trio-sum-equal? trio s)
  (= (+ (car trio) (cadr trio) (caddr trio)) s))

; 和がsに等しい3つ組をフィルターする
(define (list-trio-sum-equal s n)
  (filter (lambda (trio) (trio-sum-equal? trio s))
          (enumerate-interval-trio 1 n)))

(list-trio-sum-equal 12 8)
