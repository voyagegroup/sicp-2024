#lang sicp


(define (require p)
  (if (not p) (amb)))


(define nouns '(noun student professor cat class))


(define verbs '(verb studies lectures eats sleeps))


(define prepositions '(prep for to in by with))


(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        ; (parse-word verbs)))
        (parse-verb-phrase)))


#|
(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))
|#

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())


(define (parse input)
  (set! *unparsed* input)
  (display 'unparsed-init:)
  (display input)(newline)
  (let ((sent (parse-sentence)))
    (display 'sent:)
    (display sent)(newline)
    (require (null? *unparsed*))
    sent))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))



(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))


(parse '(the cat eats))
; (parse '(the student with the cat sleeps in the class))
; (parse '(the professor lectures to the student in the class with the cat))

#|
副作用を持つため。
parse-sentenceは、(parse-noun-phrase) → (parse-verb-phrase)の順番であり、名詞→動詞の評価順を意図している。
(parse-word) が、*unparsed*の中身を変えてしまうため、順番をたとえば、右から左の動詞→名詞の評価順にすると評価ができなくなってしまう。
具体的には、(parse-verb-phrase)がtheを読もうとして。
|#










