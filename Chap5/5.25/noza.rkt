#lang sicp

(#%require (file "../0.0/noza.rkt")
           (file "../0.0/noza-evaluator-support.rkt"))

(define the-global-environment (setup-environment))

(define (get-global-environment)
  the-global-environment)

; 問題 5.24: cond 式と else 節を識別する。
(define (cond? exp)
  (and (pair? exp) (eq? (car exp) 'cond)))

(define (cond-else-clause? clause)
  (eq? (car clause) 'else))

; 問題 5.25: 引数の式と呼び出し元の環境をサンクに保存する。
; 利用者のデータと区別できるよう、タグは非公開のオブジェクトにする。
(define thunk-tag (list 'thunk))
(define evaluated-thunk-tag (list 'evaluated-thunk))

(define (delay-arguments operands env)
  (map (lambda (exp) (vector thunk-tag exp env)) operands))

(define (thunk? value)
  (and (vector? value)
       (= (vector-length value) 3)
       (eq? (vector-ref value 0) thunk-tag)))

(define (evaluated-thunk? value)
  (and (vector? value)
       (= (vector-length value) 3)
       (eq? (vector-ref value 0) evaluated-thunk-tag)))

(define (thunk-exp thunk) (vector-ref thunk 1))
(define (thunk-env thunk) (vector-ref thunk 2))
(define (thunk-value thunk) (vector-ref thunk 1))

(define (memoize-thunk! thunk value)
  (vector-set! thunk 1 value)
  (vector-set! thunk 2 #f)
  (vector-set! thunk 0 evaluated-thunk-tag))

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
        (list 'user-print user-print)
        ; 問題 5.24: cond の節を取り出す演算を登録する。
        (list 'cond? cond?)
        (list 'cond-clauses cdr)
        (list 'cond-no-clauses? null?)
        (list 'cond-first-clause car)
        (list 'cond-rest-clauses cdr)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'cond-predicate car)
        (list 'cond-actions cdr)
        (list 'cond-no-actions? null?)
        ; 問題 5.25: サンクの作成、参照、メモ化を登録する。
        (list 'delay-arguments delay-arguments)
        (list 'thunk? thunk?)
        (list 'evaluated-thunk? evaluated-thunk?)
        (list 'thunk-exp thunk-exp)
        (list 'thunk-env thunk-env)
        (list 'thunk-value thunk-value)
        (list 'memoize-thunk! memoize-thunk!)))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(
    read-eval-print-loop
      (perform (op initialize-stack))
      (perform
       (op prompt-for-input) (const ";;; L-Eval input:"))
      (assign exp (op read))
      (test (op eof-object?) (reg exp))
      (branch (label evaluator-done))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      ; 問題 5.25: 表示する結果は強制する。
      (goto (label actual-value))

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
      ; 問題 5.24: 手続き適用と判定する前に cond を振り分ける。
      (test (op cond?) (reg exp))
      (branch (label ev-cond))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))

    ; 問題 5.25: 評価の結果がサンクなら、実際の値が得られるまで強制する。
    actual-value
      (save continue)
      (assign continue (label actual-value-done))
      (goto (label eval-dispatch))

    actual-value-done
      (restore continue)
      (goto (label force-it))

    force-it
      (test (op evaluated-thunk?) (reg val))
      (branch (label force-memoized-thunk))
      (test (op thunk?) (reg val))
      (branch (label force-thunk))
      (goto (reg continue))

    force-memoized-thunk
      (assign val (op thunk-value) (reg val))
      (goto (reg continue))

    force-thunk
      (save continue)
      (save val)
      (assign exp (op thunk-exp) (reg val))
      (assign env (op thunk-env) (reg val))
      (assign continue (label force-thunk-done))
      (goto (label actual-value))

    force-thunk-done
      (restore exp)
      (perform (op memoize-thunk!) (reg exp) (reg val))
      (restore continue)
      (goto (reg continue))

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
      ; 問題 5.25: 演算子が遅延されていても手続きを取り出す。
      (goto (label actual-value))

    ev-appl-did-operator
      (restore unev) ; 被演算子
      (restore env)
      (assign proc (reg val)) ; 演算子
      (goto (label apply-dispatch))

    ; 問題 5.25: 基本手続きの引数だけを先に強制する。
    ev-appl-primitive-args
      (assign argl (op empty-arglist))
      (test (op no-operands?) (reg unev))
      (branch (label primitive-apply))
      (save proc)

    ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label actual-value))

    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label actual-value))

    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label primitive-apply))

    apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label ev-appl-primitive-args))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))

    primitive-apply
      (assign val (op apply-primitive-procedure)
                  (reg proc)
                  (reg argl))
      (restore continue)
      (goto (reg continue))

    ; 問題 5.25: 合成手続きには未評価の引数をサンクとして渡す。
    compound-apply
      (assign argl (op delay-arguments) (reg unev) (reg env))
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
      ; 問題 5.25: 分岐の判定前に述語を強制する。
      (goto (label actual-value)) ; 述語を評価

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

    ; 問題 5.24: 述語を順に評価し、選んだ節の本体を ev-sequence に渡す。
    ev-cond
      (save continue)
      (assign unev (op cond-clauses) (reg exp))

    ev-cond-loop
      (test (op cond-no-clauses?) (reg unev))
      (branch (label ev-cond-no-match))
      (assign exp (op cond-first-clause) (reg unev))
      (test (op cond-else-clause?) (reg exp))
      (branch (label ev-cond-else))
      (save unev)
      (save env)
      (assign continue (label ev-cond-decide))
      (assign exp (op cond-predicate) (reg exp))
      ; 問題 5.25: 分岐の判定前に述語を強制する。
      (goto (label actual-value))

    ev-cond-decide
      (restore env)
      (restore unev)
      (test (op true?) (reg val))
      (branch (label ev-cond-match))
      (assign unev (op cond-rest-clauses) (reg unev))
      (goto (label ev-cond-loop))

    ev-cond-match
      (assign exp (op cond-first-clause) (reg unev))
      (assign unev (op cond-actions) (reg exp))
      (test (op cond-no-actions?) (reg unev))
      (branch (label ev-cond-return))
      (goto (label ev-sequence))

    ev-cond-else
      (assign unev (op cond-rest-clauses) (reg unev))
      (test (op cond-no-clauses?) (reg unev))
      (branch (label ev-cond-else-actions))
      (assign val (const cond-else-not-last-error))
      (goto (label signal-error))

    ev-cond-else-actions
      (assign unev (op cond-actions) (reg exp))
      (test (op cond-no-actions?) (reg unev))
      (branch (label ev-cond-empty-else))
      (goto (label ev-sequence))

    ev-cond-empty-else
      (assign val (const cond-empty-else-error))
      (goto (label signal-error))

    ev-cond-no-match
      (assign val (const #f))

    ev-cond-return
      (restore continue)
      (goto (reg continue))

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
      (perform
       (op announce-output) (const ";;; L-Eval value:"))
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

