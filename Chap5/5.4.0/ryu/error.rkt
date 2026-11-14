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


(define (contains-zero? numbers)
  (cond ((null? numbers) false)
        ((= (car numbers) 0) true)
        (else (contains-zero? (cdr numbers)))))

(define (safe-divide . args)
  (cond ((null? args)
         (make-evaluator-error 'wrong-number-of-argments '/))
        ((contains-zero?
          (if (null? (cdr args)) args (cdr args)))
         (make-evaluator-error 'division-by-zero args))
        (else (apply / args))))
        
