#lang sicp
(#%require (file "noza.rkt"))

;;; 問題 4.49 の問題を示し、問題 4.50 の ramb で解決することを示す。
;;;
;;; 問題 4.49:
;;;   parse-word を「入力を無視して語リストから amb で選ぶ」版にすると、
;;;   maybe-extend が常に再帰側を先に試みるため無限再帰に陥る。
;;;
;;; 問題 4.50:
;;;   maybe-extend の amb を ramb に変えることで、ランダムに基底ケースが
;;;   選ばれて再帰が止まり、多様な文を生成できる。

(define nouns        '(noun student professor cat class))
(define verbs        '(verb studies lectures eats sleeps))
(define articles     '(article the a))
(define prepositions '(prep for to in by with))

;;; 語リストから ramb 的にランダムで語を一つ選ぶ
;;; (shuffle は noza.rkt で定義済み)
(define (pick-word word-list)
  (let ((words (shuffle (cdr word-list))))
    (list (car word-list) (car words))))

;;; ramb の「ランダムに基底か再帰かを選ぶ」動作を coin-flip で表現する
(define (coin-flip) (= (random 2) 0))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (pick-word prepositions)
        (parse-noun-phrase)))

;;; ramb 版 maybe-extend: ランダムに止まるので無限再帰しない
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (if (coin-flip)
        noun-phrase
        (maybe-extend (list 'noun-phrase
                            noun-phrase
                            (parse-prepositional-phrase)))))
  (maybe-extend (list 'simple-noun-phrase
                      (pick-word articles)
                      (pick-word nouns))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (if (coin-flip)
        verb-phrase
        (maybe-extend (list 'verb-phrase
                            verb-phrase
                            (parse-prepositional-phrase)))))
  (maybe-extend (pick-word verbs)))

(define (generate-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(display "=== 問題 4.50: ramb による文生成 (問題 4.49 の解決) ===") (newline)
(let loop ((i 0))
  (if (< i 5)
      (begin
        (display (generate-sentence)) (newline)
        (loop (+ i 1)))
      'done))
