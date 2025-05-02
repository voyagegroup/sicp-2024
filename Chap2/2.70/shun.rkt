#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol c tree)
  (if (leaf? tree)
      '()
      (cond ((in-symbols c (symbols (left-branch tree)))
             (cons 0 (encode-symbol c (left-branch tree))))
            ((in-symbols c (symbols (right-branch tree)))
             (cons 1 (encode-symbol c (right-branch tree))))
             )))

(define (in-symbols c symbols)
  (cond ((null? symbols) false)
        ((equal? c (car symbols)) true)
        (else (in-symbols c (cdr symbols)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; 記号
                               (cadr pair))  ; 頻度
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge sets)
  (if (= (length sets) 1)
      (car sets)
      (let ((first (car sets))
            (second (cadr sets))
            (rest (cddr sets)))
        (successive-merge 
         (adjoin-set (make-code-tree first second) rest)))))

(define kasi-tree
  (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define kasi
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

kasi-tree
; '((leaf NA 16) ((leaf YIP 9) (((leaf A 2) ((leaf WAH 1) (leaf BOOM 1) (WAH BOOM) 2) (A WAH BOOM) 4) ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) (JOB GET) 4) (SHA JOB GET) 7) (A WAH BOOM SHA JOB GET) 11) (YIP A WAH BOOM SHA JOB GET) 20) (NA YIP A WAH BOOM SHA JOB GET) 36)

(define encoded (encode kasi kasi-tree))

encoded
; '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(length encoded)
; 84

; 八記号アルファベットの固定長符号を使うとこの歌を符号化するのに必要な最小ビット数はいくらか.
; 36 * 3 = 108