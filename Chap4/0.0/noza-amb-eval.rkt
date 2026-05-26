#lang sicp

;;; SICP 4.3.3 amb Evaluator
;;; analyze-assignment が set! をバックトラック時に自動で巻き戻す。
;;; これにより *unparsed* のようなグローバル変数が教科書どおりに動く。

(#%provide ambeval the-global-environment amb-all amb-take ambeval-load!)

;;; ============================================================
;;; 環境
;;; ============================================================

(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame)    (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define the-empty-environment '())

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (cdr env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (car env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (cdr env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (car env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (car env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;;; ============================================================
;;; 基本手続き
;;; ============================================================

(define primitive-procedures
  (list (list 'car car) (list 'cdr cdr) (list 'cons cons)
        (list 'null? null?) (list 'list list) (list 'not not)
        (list 'memq memq) (list 'memv memv) (list 'member member)
        (list 'length length) (list 'append append) (list 'reverse reverse)
        (list '= =) (list '< <) (list '> >) (list '<= <=) (list '>= >=)
        (list '+ +) (list '- -) (list '* *) (list '/ /)
        (list 'abs abs) (list 'eq? eq?) (list 'equal? equal?)
        (list 'display display) (list 'newline newline)
        (list 'cadr cadr) (list 'caddr caddr) (list 'caar caar)
        (list 'caaar caaar) (list 'caadr caadr) (list 'cddr cddr)
        (list 'cadddr cadddr)
        (list 'number? number?) (list 'symbol? symbol?) (list 'pair? pair?)
        (list 'boolean? boolean?) (list 'string? string?) (list 'list? list?)
        (list 'even? even?) (list 'odd? odd?) (list 'zero? zero?)
        (list 'remainder remainder) (list 'quotient quotient) (list 'modulo modulo)
        (list 'gcd gcd) (list 'lcm lcm)
        (list 'assoc assoc) (list 'assq assq) (list 'assv assv)
        (list 'map map) (list 'for-each for-each)
        (list 'min min) (list 'max max)
        (list 'sqrt sqrt) (list 'expt expt)
        (list 'set-car! set-car!) (list 'set-cdr! set-cdr!)
        (list 'error error)))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (setup-environment)
  (let ((env (extend-environment
              (map car primitive-procedures)
              (map (lambda (p) (list 'primitive (cadr p)))
                   primitive-procedures)
              the-empty-environment)))
    (define-variable! 'true  true  env)
    (define-variable! 'false false env)
    env))

(define the-global-environment (setup-environment))

;;; ============================================================
;;; 構文補助
;;; ============================================================

(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp) tag) false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)           (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)         (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp)    (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (lambda? exp)          (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp)       (cddr exp))
(define (make-lambda params body) (cons 'lambda (cons params body)))

(define (if? exp)         (tagged-list? exp 'if))
(define (if-predicate exp)   (cadr exp))
(define (if-consequent exp)  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))
(define (make-if pred cons alt) (list 'if pred cons alt))

(define (begin? exp)        (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq)
  (if (null? (cdr seq)) (car seq) (cons 'begin seq)))
(define (last-exp? seq) (null? (cdr seq)))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond->if exp)
  (define (expand clauses)
    (if (null? clauses)
        'false
        (let ((first (car clauses)) (rest (cdr clauses)))
          (if (eq? (car first) 'else)
              (if (null? rest)
                  (make-begin (cdr first))
                  (error "else not last"))
              (make-if (car first)
                       (make-begin (cdr first))
                       (expand rest))))))
  (expand (cdr exp)))

(define (let? exp)  (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))
(define (and? exp)  (tagged-list? exp 'and))
(define (or? exp)   (tagged-list? exp 'or))

(define (let->lambda exp)
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (cons (make-lambda (map car bindings) body)
          (map cadr bindings))))

(define (let*->nested-lets exp)
  (define (expand bindings body)
    (if (null? bindings)
        (cons 'begin body)
        (list 'let (list (car bindings))
              (expand (cdr bindings) body))))
  (expand (cadr exp) (cddr exp)))

(define (and->if exp)
  (if (null? (cdr exp)) 'true
      (if (null? (cddr exp))
          (cadr exp)
          (make-if (cadr exp) (cons 'and (cddr exp)) 'false))))

(define (or->if exp)
  (if (null? (cdr exp)) 'false
      (make-if (cadr exp) (cadr exp) (cons 'or (cddr exp)))))

(define (application? exp) (pair? exp))
(define (operator exp)     (car exp))
(define (operands exp)     (cdr exp))

(define (amb? exp)        (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;;; ============================================================
;;; 合成手続き
;;; ============================================================

(define (make-procedure params bproc env) (list 'procedure params bproc env))
(define (compound-procedure? proc)        (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-body proc)        (caddr proc))
(define (procedure-environment proc) (cadddr proc))

;;; ============================================================
;;; analyze（実行手続きの生成） ― 4.3.3 に基づく CPS 版
;;; ============================================================

(define (analyze exp)
  (cond
    ((self-evaluating? exp) (analyze-self-evaluating exp))
    ((quoted? exp)          (analyze-quoted exp))
    ((variable? exp)        (analyze-variable exp))
    ((assignment? exp)      (analyze-assignment exp))
    ((definition? exp)      (analyze-definition exp))
    ((if? exp)              (analyze-if exp))
    ((lambda? exp)          (analyze-lambda exp))
    ((begin? exp)           (analyze-sequence (begin-actions exp)))
    ((cond? exp)            (analyze (cond->if exp)))
    ((let? exp)             (analyze (let->lambda exp)))
    ((let*? exp)            (analyze (let*->nested-lets exp)))
    ((and? exp)             (analyze (and->if exp)))
    ((or? exp)              (analyze (or->if exp)))
    ((amb? exp)             (analyze-amb exp))
    ((application? exp)     (analyze-application exp))
    (else (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars  (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-val fail2)
               (if (true? pred-val)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (true? x) (not (eq? x false)))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env (lambda (a-val fail2) (b env succeed fail2)) fail)))
  (define (loop first rest)
    (if (null? rest) first
        (loop (sequentially first (car rest)) (cdr rest))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var   (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

;;; 4.3.3 の核心: set! の値をバックトラック時に元の値へ復元する
(define (analyze-assignment exp)
  (let ((var   (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-val (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-val env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((pproc  (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (proc fail2)
               (get-args aprocs env
                         (lambda (args fail3)
                           (execute-application proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs) env
                                (lambda (args fail3)
                                  (succeed (cons arg args) fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond
    ((primitive-procedure? proc)
     (succeed (apply-primitive-procedure proc args) fail))
    ((compound-procedure? proc)
     ((procedure-body proc)
      (extend-environment (procedure-parameters proc) args
                          (procedure-environment proc))
      succeed fail))
    (else (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

;;; analyze-amb: 各選択肢を順に試み、使い果たしたら fail を呼ぶ
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env succeed
                           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;;; ============================================================
;;; ambeval トップレベル
;;; ============================================================

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;; ============================================================
;;; ヘルパー
;;; ============================================================

(define (ambeval-load! exp)
  (ambeval exp the-global-environment
           (lambda (val fail) val)
           (lambda () (error "ambeval-load! failed" exp))))

;;; 全解をリストで返す（解が有限個の場合のみ）
(define (amb-all exp)
  (let ((results '()))
    (ambeval exp the-global-environment
             (lambda (val fail)
               (set! results (cons val results))
               (fail))
             (lambda () 'done))
    (reverse results)))

;;; 最大 n 個の解をリストで返す
(define (amb-take n exp)
  (let ((results '())
        (count   0))
    (call-with-current-continuation
     (lambda (escape)
       (ambeval exp the-global-environment
                (lambda (val fail)
                  (set! results (cons val results))
                  (set! count (+ count 1))
                  (if (>= count n)
                      (escape 'done)
                      (fail)))
                (lambda () 'done))))
    (reverse results)))
