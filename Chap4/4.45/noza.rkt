#lang sicp
(#%require (file "../0.0/noza-amb-eval.rkt"))

(ambeval-load! '(define nouns       '(noun student professor cat class)))
(ambeval-load! '(define verbs       '(verb studies lectures eats sleeps)))
(ambeval-load! '(define articles    '(article the a)))
(ambeval-load! '(define prepositions '(prep for to in by with)))

(ambeval-load! '(define *unparsed* '()))

(ambeval-load! '(define (require p) (if (not p) (amb) 'ok)))

(ambeval-load! '(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word))))

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
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase))))

(ambeval-load! '(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent)))

;;; 5通りの構文解析を取得
(define results
  (amb-all '(parse '(the professor lectures to the student in the class with the cat))))

(display (string-append "解の数: " (number->string (length results))))
(newline) (newline)
(for-each (lambda (r) (display r) (newline) (newline)) results)
