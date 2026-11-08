#lang sicp

(#%require (file "../0.0/noza.rkt")
           (file "../0.0/noza-evaluator-support.rkt"))

(define the-global-environment (setup-environment))

(define (get-global-environment)
  the-global-environment)

(define eceval-operations
  (list (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'eof-object? eof-object?)
        (list 'get-global-environment get-global-environment)
        (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'announce-output announce-output)
        (list 'user-print user-print)))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
    read-eval-print-loop
      (perform (op initialize-stack))
      (perform
       (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (test (op eof-object?) (reg exp))
      (branch (label evaluator-done))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label eval-dispatch))

    eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))
      (test (op variable?) (reg exp))
      (branch (label ev-variable))
      (test (op quoted?) (reg exp))
      (branch (label ev-quoted))
      (test (op assignment?) (reg exp))
      (branch (label ev-assignment))
      (test (op definition?) (reg exp))
      (branch (label ev-definition))
      (test (op if?) (reg exp))
      (branch (label ev-if))
      (test (op lambda?) (reg exp))
      (branch (label ev-lambda))
      (test (op begin?) (reg exp))
      (branch (label ev-begin))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))

    ; 単純式の評価
    ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))

    ev-variable
      (assign val (op lookup-variable-value) (reg exp) (reg env))
      (goto (reg continue))

    ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))

    ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure)
                  (reg unev) (reg exp) (reg env))
      (goto (reg continue))

    ev-application
      (save continue)
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label eval-dispatch))

    ev-appl-did-operator
      (restore unev) ; 被演算子
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val)) ; 演算子
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)

    ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label eval-dispatch))

    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))

    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))

    apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))

    primitive-apply
      (assign val (op apply-primitive-procedure)
                  (reg proc)
                  (reg argl))
      (restore continue)
      (goto (reg continue))

    compound-apply
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment)
                  (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))

    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))

    ev-sequence
      (assign exp (op first-exp) (reg unev))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))

    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))

    ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))

    ev-if
      (save exp)
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch)) ; 述語を評価

    ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))

    ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))

    ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))

    ev-assignment
      (assign unev (op assignment-variable) (reg exp))
      (save unev) ; 後のために変数を退避
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))

    ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform
       (op set-variable-value!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

    ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev) ; 後のために変数を退避
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch)) ; 定義する値を評価

    ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform
       (op define-variable!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

    print-result
      ; 問題 5.26: 今回の入力のスタック統計を表示する。
      (perform (op print-stack-statistics))
      (perform
       (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))

    unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))

    unknown-procedure-type
      (restore continue) ; スタックを掃除する
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))

    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))

    evaluator-done
   )))

(start eceval)

; 問題 5.26: 起動後、以下の式をコメント記号を除いて順に入力する。
;
; (define (factorial n)
;   (define (iter product counter)
;     (if (> counter n)
;         product
;         (iter (* counter product)
;               (+ counter 1))))
;   (iter 1 1))
;
; (factorial 1)
; (factorial 2)
; (factorial 3)
; (factorial 4)
; (factorial 5)
; (factorial 10)
;
; 定義時の統計は除き、各 factorial 呼び出しの total-pushes と
; maximum-depth を記録する。
