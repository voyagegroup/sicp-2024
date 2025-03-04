#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (filter seqs (list)))
            (accumulate-n op init (filter-2 seqs (list))))))


(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; --- 各seqsの最初の要素を取ってsequenceにする ---
(define (filter seqs sequences)
  (if (null? seqs)
      sequences
      (filter (cdr seqs) (append sequences (list (car (car seqs)))))))

(filter s (list))
; (1 4 7 10)

; --- 各seqsの最初の要素をのぞいたseqsを返す ---
(define (filter-2 old-seqs new-seqs)
  (if (null? old-seqs)
      new-seqs
      (filter-2 (cdr old-seqs) (append new-seqs (list (cdr (car old-seqs)))))))
(filter-2 s (list))
; ((2 3) (5 6) (8 9) (11 12))

(accumulate-n + 0 s)
; (22 26 30)