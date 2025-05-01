#lang racket

; 本文のコピー
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

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; 方針
;; 1. tree が葉ならば、空リストを返す。
;; 2. tree が葉でなく、symbol が左の枝に含まれているならば、0 を cons して左の枝を再帰的に呼び出す。
;; 3. tree が葉でなく、symbol が右の枝に含まれているならば、1 を cons して右の枝を再帰的に呼び出す。
;; 4. tree が葉でなく、symbol が左の枝にも右の枝にも含まれていないならば、エラーを返す。
(define (encode-symbol symbol tree)
    (if (leaf? tree)
        '()
        (cond ((element-of-set? symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
              ((element-of-set? symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
              (else (error "bad symbol -- ENCODE-SYMBOL" symbol)))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(A D A B B C A))

(encode sample-message sample-tree)
