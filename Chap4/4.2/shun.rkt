#lang sicp

; a
; 以下はtrueになってしまうため、定義もapplicationとして認識されてしまう

(define (application? exp) (pair? exp))
(application? (list 'define 'x 3)) ; => #t

; b
; 以下のようにapplication?の定義を変更する
(define (application? exp) (tagged-list? exp 'call))

(define (eval exp env)
  (cond ((application? exp)
         (my-apply (eval (cadr exp) env)
                (list-of-values (operands (cdr exp)) env)))
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        
        (else
         (error "Unknown expression type -- EVAL" exp))))