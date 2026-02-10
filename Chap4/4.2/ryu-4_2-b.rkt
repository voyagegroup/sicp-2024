#lang sicp

(define (eval exp env)
  (cond
    ((call? exp) ; application? → call?
     (apply (eval (operator (cdr exp)) env)
            (list-of-values (operands (cdr exp) env))))

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

(define (call? exp) (tagged-list? exp 'call))
