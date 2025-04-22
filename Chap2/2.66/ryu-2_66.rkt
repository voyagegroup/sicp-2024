#lang sicp
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))


(define (make-tree entry left right)
  (list entry left right))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; -- ここから
(define (make-record k v) (cons k v))
(define (key record) (car record))
(define (value record) (cdr record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

(define records (list->tree (list
                  (make-record 1 "taro")
                  (make-record 2 "jiro")
                  (make-record 3 "saburo")
                  (make-record 5 "goro"))))

records
(lookup 1 records)
(lookup 3 records)
(lookup 4 records)
(lookup 5 records)
; ((1 . "taro") (2 . "jiro") (3 . "saburo"))
; (1 . "taro")
; (3 . "saburo")
; #f
; (5 . "goro")
