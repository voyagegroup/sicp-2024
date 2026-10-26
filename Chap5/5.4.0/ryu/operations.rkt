; 5.4

; -- eceval-operationsでつかうやつ
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (if? exp)
  (tagged-list? exp 'if))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (application? exp)
  (pair? exp))

(define (enclosing-environment env)
  (cdr env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (text-of-quotation exp)
  (cadr exp))

; lambda式
(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

; 複合手続き
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

; 手続き適用
(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

; 引数リストの初期化
(define (empty-arglist)
  '())

; 引数の評価
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (last-operand? ops) (null? (cdr ops)))
(define (rest-operands ops) (cdr ops))

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

; 手続きオブジェクト
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-environment proc)
  (cadddr proc))

(define (apply-primitive-procedure proc args)
  (apply (cadr proc) args))

; 式の列
(define (begin-actions exp)
  (cdr exp))

(define (no-more-exps? seq)
  (null? seq))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

; if式
(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false
      (cadddr exp)))

(define (true? x)
  (not (eq? x false)))

; define式
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; set!式
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; 入出力
(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

; 5.23 cond
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? clause)
  (eq? (car clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        (else (cons 'begin seq))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"))
            (list 'if
                  (cond-predicate first)
                  (sequence->exp (cond-actions first))
                  (expand-clauses rest))))))

; 5.23 cond

; --- eceval-operations

(define eceval-operations
  (list
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
   (list 'operator operator)
   (list 'operands operands)
   (list 'empty-arglist empty-arglist)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'last-operand? last-operand?)
   (list 'rest-operands rest-operands)
   (list 'adjoin-arg adjoin-arg)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'compound-procedure? compound-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-environment procedure-environment)
   (list 'procedure-body procedure-body)
   (list 'extend-environment extend-environment)
   (list 'begin-actions begin-actions)
   (list 'no-more-exps? no-more-exps?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'if-predicate if-predicate)
   (list 'true? true?)
   (list 'if-alternative if-alternative)
   (list 'if-consequent if-consequent)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'prompt-for-input prompt-for-input)
   (list 'read read)
   (list 'get-global-environment get-global-environment)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'cond? cond?)
   (list 'cond->if cond->if)
   ))



