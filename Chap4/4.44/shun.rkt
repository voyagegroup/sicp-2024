#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? li)
  (define (has-dup n li)
    (if (null? li)
        #f
        (if (= n (car li))
            #t
            (has-dup n (cdr li)))))
  (define (distinct-itr li)
    (if (null? li)
        #t
        (if (has-dup (car li) (cdr li))
            #f
            (distinct-itr (cdr li)))))
  (distinct-itr li))

(define (safe? k positions) 
 (define (safe-itr x n)
   (or (= n k)
       (let ((y (list-ref positions n)))
            (and (not (= x y)) ; 上の行にいないか
                 (not (= (abs (- x y)) n)) ; xとyの絶対値は斜めにいるかを表す
                 (safe-itr x (+ n 1))))))
 (safe-itr (car positions) 1))

(define (eight-queen)
  (let ((a (amb 1 2 3 4 5 6 7 8)))
    (let ((b (amb 1 2 3 4 5 6 7 8)))
      (require (safe? 2 (list b a)))
      (let ((c (amb 1 2 3 4 5 6 7 8)))
        (require (safe? 3 (list c b a)))
        (let ((d (amb 1 2 3 4 5 6 7 8)))
          (require (safe? 4 (list d c b a)))
          (let ((e (amb 1 2 3 4 5 6 7 8)))
            (require (safe? 5 (list e d c b a)))
            (let ((f (amb 1 2 3 4 5 6 7 8)))
              (require (safe? 6 (list f e d c b a)))
              (let ((g (amb 1 2 3 4 5 6 7 8)))
                (require (safe? 7 (list g f e d c b a)))
                (let ((h (amb 1 2 3 4 5 6 7 8)))
                  (require (safe? 8 (list h g f e d c b a)))
                  (list h g f e d c b a))))))))))

(length (stream->list
 (in-amb
  (eight-queen))))

;92