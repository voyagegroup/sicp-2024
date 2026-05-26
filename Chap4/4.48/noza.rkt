#lang sicp
(#%require (file "../0.0/noza-amb-eval.rkt"))

;;; 問題 4.48: 文法を拡張し形容詞・副詞・合成文を扱えるようにする。

(ambeval-load! '(define nouns        '(noun student professor cat class)))
(ambeval-load! '(define verbs        '(verb studies lectures eats sleeps)))
(ambeval-load! '(define articles     '(article the a)))
(ambeval-load! '(define prepositions '(prep for to in by with)))
(ambeval-load! '(define adjectives   '(adj big small lazy happy hungry)))
(ambeval-load! '(define adverbs      '(adv quickly slowly happily loudly)))
(ambeval-load! '(define conjunctions '(conj and but)))

(ambeval-load! '(define *unparsed* '()))
(ambeval-load! '(define (require p) (if (not p) (amb) 'ok)))

(ambeval-load! '(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word))))

;;; article [adj] noun
(ambeval-load! '(define (parse-simple-noun-phrase)
  (let ((art (parse-word articles)))
    (amb (list 'simple-noun-phrase art (parse-word nouns))
         (let ((adj (parse-word adjectives)))
           (list 'simple-noun-phrase art adj (parse-word nouns)))))))

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

;;; verb [adv]
(ambeval-load! '(define (parse-verb-base)
  (let ((v (parse-word verbs)))
    (amb v
         (list 'adv-verb v (parse-word adverbs))))))

(ambeval-load! '(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-verb-base))))

(ambeval-load! '(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase))))

;;; sentence (conj sentence)*
(ambeval-load! '(define (parse-compound-sentence)
  (define (maybe-extend sent)
    (amb sent
         (maybe-extend (list 'compound-sentence
                             sent
                             (parse-word conjunctions)
                             (parse-sentence)))))
  (maybe-extend (parse-sentence))))

(ambeval-load! '(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-compound-sentence)))
    (require (null? *unparsed*))
    sent)))

;;; 動作確認
(define (parse-first input)
  (let ((results (amb-take 1 (list 'parse (list 'quote input)))))
    (if (null? results) 'no-parse (car results))))

(display "--- the big cat eats quickly ---") (newline)
(display (parse-first '(the big cat eats quickly))) (newline) (newline)
(display "--- the happy student lectures slowly to the class ---") (newline)
(display (parse-first '(the happy student lectures slowly to the class))) (newline) (newline)
(display "--- the professor eats and the cat sleeps ---") (newline)
(display (parse-first '(the professor eats and the cat sleeps))) (newline) (newline)
(display "--- a small student studies but the lazy cat eats ---") (newline)
(display (parse-first '(a small student studies but the lazy cat eats))) (newline)
