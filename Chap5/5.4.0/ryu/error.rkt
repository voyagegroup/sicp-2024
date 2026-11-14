; errorのタグ
(define evaluator-error-tag
  (list 'evaluator-error))

(define (make-evaluator-error kind details)
  (list evaluator-error-tag kind details))

(define (evaluator-error? value)
  (and (pair? value)
       (eq? (car value) evaluator-error-tag)))

; 表示用でtagを消す
(define (evaluator-error-message value)
  (cdr value))
        
