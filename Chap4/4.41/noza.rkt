#lang racket

; 問題 4.41
; 通常の Scheme で多住居パズルを解く

; リストから要素 x を除いたリストを返す
(define (remove x lst)
  (filter (lambda (y) (not (= x y))) lst))

; リストの全順列を生成する
(define (permutations lst)
  (if (null? lst)
      '(())
      (append-map (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x lst))))
                  lst)))

(define (multiple-dwelling)
  (filter
   (lambda (perm)
     (let ((baker    (list-ref perm 0))
           (cooper   (list-ref perm 1))
           (fletcher (list-ref perm 2))
           (miller   (list-ref perm 3))
           (smith    (list-ref perm 4)))
       (and (not (= baker 5))
            (not (= cooper 1))
            (not (= fletcher 5))
            (not (= fletcher 1))
            (> miller cooper)
            (not (= (abs (- smith fletcher)) 1))
            (not (= (abs (- fletcher cooper)) 1)))))
   (permutations '(1 2 3 4 5))))

(define (format-result perm)
  (list (list 'baker    (list-ref perm 0))
        (list 'cooper   (list-ref perm 1))
        (list 'fletcher (list-ref perm 2))
        (list 'miller   (list-ref perm 3))
        (list 'smith    (list-ref perm 4))))

(map format-result (multiple-dwelling))
