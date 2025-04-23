#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (if (leaf? tree)
        '()
        (cond ((element-of-set? symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
              ((element-of-set? symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
              (else (error "bad symbol -- ENCODE-SYMBOL" symbol)))))

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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let ((smallest (car set))
            (next-smallest (cadr set)))
        (successive-merge
         (adjoin-set
          (make-code-tree smallest next-smallest)
          (cddr set))))))

;; 木の生成
(define tree
  (generate-huffman-tree
   '((a 2)
     (A 2)
     (Na 16)
     (na 16)
     (Boom 1)
     (boom 1)
     (Sha 3)
     (sha 3)
     (get 2)
     (Get 2)
     (Yip 9)
     (yip 9)
     (Job 2)
     (job 2)
     (Wah 1)
     (wah 1))))

;; 木の生成
(define tree-1
  (generate-huffman-tree
   '((a 2)
     (na 16)
     (boom 1)
     (Sha 3)
     (Get 2)
     (yip 9)
     (job 2)
     (Wah 1))))

(define message
  '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(encode message tree)
(encode message tree-1)
; Get
; a
; job
; Sha
; na
; Wah
; yip
; boom
; 出てくる文字は８種類
; よって固定長で必要なビット数は3ビット
; 単語数は 36
; 36 * 3 = 108ビットが最小で必要
