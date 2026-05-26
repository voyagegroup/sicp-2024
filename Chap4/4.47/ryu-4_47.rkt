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

(define *unparsed* '())


(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))



#|
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
|#
; 4.47
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))


(define (parse-word word-list)
  (display 'unparsed:)
  (display *unparsed*)
  (newline)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))


(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))


; (parse '(the cat eats))
; (parse '(the student with the cat sleeps in the class))
(parse '(the professor lectures to the student in the class with the cat))

#|
実行したら、処理が終わらなかった。

(parse-verb-phrase) は内部で、(parse-word)のよびだしと、自身の再帰をしている。


parse-wordの中でprintデバッグをしたら以下の出力でるーぷした
unparsed:(to the student in the class with the cat)
unparsed:(to the student in the class with the cat)
unparsed:(to the student in the class with the cat)
unparsed:(to the student in the class with the cat)
unparsed:(to the student in the class with the cat)
unparsed:(to the student in the class with the cat)

parse-wordの処理を追うと、
unparsedの先頭がword-listにあるか、null?じゃないかをrequireしている
found-wordを定義し、unparsedには、found-wordがとりのぞかれたcdrの値に更新される。
しかし、unparsedの先頭に動詞がヒットしなかった場合、unparsedのトークンは消費されずに処理が進む。
今回の場合、先頭がtoだったので、動詞リストにヒットしていなかった。
そして、parse-verb-wordは自身を再帰しているため、無限ループに入っていた。

前提をかんがえると、
Louis版は、動詞句を「動詞」または「動詞句 + 前置詞句」として定義している。
そのため、「lectures to the student」の部分で動詞句判定をされていた。
前置詞句を読む前に再び parse-verb-phrase を呼ぶようになっているため、ループをしていた。
|#












