#lang sicp
(#%require (file "../0.0/noza-amb-eval.rkt"))

;;; 問題 4.49: Alyssa の考えを実装する。
;;; parse-word を「入力を消費せず amb で語を生成する」版に変更するだけで
;;; 構文解析器が文生成器になる。

(ambeval-load! '(define nouns        '(noun student professor cat class)))
(ambeval-load! '(define verbs        '(verb studies lectures eats sleeps)))
(ambeval-load! '(define articles     '(article the a)))
(ambeval-load! '(define prepositions '(prep for to in by with)))

;;; リストから非決定的に一要素を選ぶ
(ambeval-load! '(define (an-element-of items)
  (if (null? items)
      (amb)
      (amb (car items) (an-element-of (cdr items))))))

;;; Alyssaの変更: 入力を無視し、amb で語を選んで返す
;;; (apply amb ...) は使えないため an-element-of で代替する
(ambeval-load! '(define (parse-word word-list)
  (list (car word-list)
        (an-element-of (cdr word-list)))))

(ambeval-load! '(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns))))

(ambeval-load! '(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase))))

(ambeval-load! '(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase))))

(ambeval-load! '(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs))))

(ambeval-load! '(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase))))

;;; 最初の5文を生成
;;; (maybe-extend は amb で「拡張しない」を先に試みるため、
;;;  最初の解は前置詞句なしの単純な文になる)
(define generated (amb-take 5 '(parse-sentence)))
(for-each (lambda (s) (display s) (newline) (newline)) generated)
