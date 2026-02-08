#lang sicp

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((op (get 'eval (operator exp))))
           (if op
               (op (operands exp) env)
               (error "Unknown expression type -- EVAL" exp))))))
