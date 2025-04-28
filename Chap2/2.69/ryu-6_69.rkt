#lang sicp

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

; make-leaf-set

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


; 対のリストを葉の順序づけられた集合へ変換する.
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; 記号
                               (cadr pair))  ; 頻度
                    (make-leaf-set (cdr pairs))))))

; make-leaf-setを触ってみる
(define my-pairs '((A 4) (B 2) (C 1) (D 1)))
my-pairs

(make-leaf-set my-pairs)
; -> ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

; 2.69 ここから
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;  successive-mergeは自分で書く手続きで, make-code-treeを使い, 集合の最小重みの要素を順に合体させ, 要素が一つになったら止める. それが目的のHuffman木である
(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set
                         (make-code-tree (car pairs) (cadr pairs))
                         (cddr pairs)))))

(generate-huffman-tree my-pairs)
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)


















