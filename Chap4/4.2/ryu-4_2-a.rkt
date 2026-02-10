#lang sicp

; もともと
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

; exp = (define x 3)
; (definition? exp) → true
(eval-definition (define x 3) env)

; (define-variable! ⟨var⟩  ⟨value⟩  ⟨env⟩ ) は環境⟨env⟩の最初のフレームに変数⟨var⟩と値⟨value⟩を対応づける新しい束縛を追加する.
(define-variable!
  (definition-variable (define x 3)) ; → x
  (eval
   (definition-value (define x 3) env) ; → 3
   env))

; (eval 3 env)
; (self-evaluating? 3) → true
; (define-variable! x 3)
; envにx=3が束縛


; --- Louis案

; applyを代入よりもまえへ
(define (eval exp env)
  (cond

    ((application? exp) ; 先頭
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))

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

(define (application? exp) (pair? exp))

; exp = (define x 3)
; (application? exp) → true
; 評価機が評価を分解するまえに、application?がtrueになってしまう
; defineだけでなく、ifやcondなどpairのものがおおいので、合成手続きとして処理されてしまう。
