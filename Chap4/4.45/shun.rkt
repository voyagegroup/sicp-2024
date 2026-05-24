#lang sicp

;;; =================================================================
;;; 1. CORE EVALUATOR
;;; =================================================================

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; このaprocの成功継続
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; get-argsの再帰呼出しの
                                ;; 成功継続
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; aを呼び出す時の成功継続
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; aを呼び出す時の失敗継続
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;; =================================================================
;;; 2. ANALYZE FUNCTIONS & SYNTAX
;;; =================================================================

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; [Self-Evaluating]
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

;; [Variables]
(define (variable? exp) (symbol? exp))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; [Quotation]
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;; [Assignment]
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

;; [Definition]
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

;; [Lambda]
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;; [If]
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; pred-valueを得るための
             ;; 述語の評価の成功継続
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; 述語の評価の失敗継続
             fail))))

;; [Begin / Sequence]
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; [Application]
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; [Cond]
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-test? clause) (eq? (cadr clause) '=>))
(define (cond-recipient clause) (caddr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF" clauses)))
              ((cond-test? first)
               (make-if (cond-predicate first)
                        (list (cond-recipient first) (cond-predicate first))
                        (expand-clauses rest)))
              (else (make-if (cond-predicate first)
                             (sequence->exp (cond-actions first))
                             (expand-clauses rest)))))))

;; [Let]
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-variables bindings) (map car bindings))
(define (let-values bindings) (map cadr bindings))
(define (make-let bindings body) (list 'let (cons bindings body)))
(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (let-variables bindings) (let-body exp))
          (let-values bindings))))

;; [And / Or] (Syntactic checks only, behavior not yet in evaluate/analyze)
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))


;;; =================================================================
;;; 3. DATA STRUCTURES (Procedures & Environments)
;;; =================================================================

;; Booleans
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; Procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Environments
(define the-empty-environment '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))


;;; =================================================================
;;; 4. PRIMITIVES & SETUP
;;; =================================================================

(define apply-in-underlying-scheme apply)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'eq? eq?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'cadr cadr)
        (list 'assoc assoc)
        (list 'list list)
        (list 'not not)
        (list 'memq memq)))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))


;;; =================================================================
;;; 5. REPL (Read-Eval-Print Loop)
;;; =================================================================

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval 成功
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval 失敗
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


;;; =================================================================
;;; 6. sentence
;;; =================================================================

(define (require p)
  (if (not p) (amb)))

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;; Start evaluator
(driver-loop)

(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb lectures) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))
;
;;;; Amb-Eval input:
;(parse '(the professor lectures to the student in the class with the cat))
;
;;;; Starting a new problem 
;;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor))
;          (verb-phrase
;           (verb-phrase
;            (verb-phrase (verb lectures)
;                         (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;            (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;           (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
; 教授が猫と一緒に教室で学生へ、講義する。
; 
;;;; Amb-Eval input:
;try-again
;
;;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor))
;          (verb-phrase
;           (verb-phrase (verb lectures)
;                        (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;           (prep-phrase (prep in)
;                        (noun-phrase (simple-noun-phrase (article the) (noun class))
;                                     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
; with the catがthe classにかかる
; 教授が、猫と一緒の教室で、学生へ講義する。
; 
;;;; Amb-Eval input:
;try-again
;
;;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor))
;          (verb-phrase
;           (verb-phrase (verb lectures)
;                        (prep-phrase (prep to)
;                                     (noun-phrase (simple-noun-phrase (article the) (noun student))
;                                                  (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
;           (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
; with the catがlecturesにかかる
; 教授が、教室にいる学生に、猫と一緒に講義する
;
;;;; Amb-Eval input:
;try-again
;
;;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor))
;          (verb-phrase
;           (verb lectures)
;           (prep-phrase (prep to) (noun-phrase
;                                   (noun-phrase (simple-noun-phrase (article the) (noun student))
;                                                (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;                                   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
; with the catがthe studentにかかる
; 教授が猫と一緒に教室にいる学生に講義する
;
;;;; Amb-Eval input:
;try-again
;
;;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor))
;          (verb-phrase
;           (verb lectures)
;           (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student))
;                                               (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class))
;                                                                                   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))
; with the catがthe classにかかる
; 教授が猫のいる教室の学生に講義する
;
;;;; Amb-Eval input:
;try-again
;
;;;; There are no more values of
;(parse '(the professor lectures to the student in the class with the cat))