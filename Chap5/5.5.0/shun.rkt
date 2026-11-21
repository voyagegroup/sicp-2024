#lang sicp

;; ============================================================
;; 5.2:レジスタ計算機シミュレータ
;; ============================================================

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing? #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if tracing?
                   (begin
                     (display (list 'register name
                                    'old contents
                                    'new value))
                     (newline))
                   'ok)
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! tracing? #t))
            ((eq? message 'trace-off)
             (set! tracing? #f))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (total-pushes 0)
        (maximum-depth 0)
        (current-depth 0))
    (define (push-value x)
      (set! s (cons x s))
      (set! total-pushes (+ total-pushes 1))
      (set! current-depth (+ current-depth 1))
      (if (> current-depth maximum-depth)
          (set! maximum-depth current-depth)
          'ok))
    (define (pop-value)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! total-pushes 0)
      (set! maximum-depth 0)
      (set! current-depth 0)
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push-value)
            ((eq? message 'pop) (pop-value))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'statistics)
             (list total-pushes maximum-depth current-depth))
            ((eq? message 'print-statistics)
             (display (list 'total-pushes total-pushes
                            'maximum-depth maximum-depth
                            'current-depth current-depth))
             (newline))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0)
        (tracing? #f)
        (breakpoints '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (breakpoint? inst)
        (member
         (cons (instruction-label inst)
               (instruction-offset inst))
         breakpoints))
      (define (execute-current)
        (let ((insts (get-contents pc)))
          (set! instruction-count (+ instruction-count 1))
          (if tracing?
              (begin
                (display (instruction-text (car insts)))
                (newline))
              'ok)
          ((instruction-execution-proc (car insts)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((breakpoint? (car insts)) 'breakpoint)
                (else
                 (execute-current)
                 (execute)))))

      ;; 現在の命令を実行してから通常実行に戻る
      (define (proceed)
        (if (null? (get-contents pc))
            'done
            (begin
              (execute-current)
              (execute))))
      (define (add-breakpoint label n)
        (set! breakpoints (cons (cons label n) breakpoints)))
      (define (remove-breakpoint label n)
        (define (iter items)
          (cond ((null? items) '())
                ((and (eq? (caar items) label)
                      (= (cdar items) n))
                 (iter (cdr items)))
                (else
                 (cons (car items) (iter (cdr items))))))
        (set! breakpoints (iter breakpoints)))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'print-instruction-count)
               (display instruction-count)
               (newline))
              ((eq? message 'init-instruction-count)
               (set! instruction-count 0))
              ((eq? message 'trace-on) ; 5.16
               (set! tracing? #t))
              ((eq? message 'trace-off)
               (set! tracing? #f))
              ((eq? message 'reg-trace-on)
               (lambda (name) ((lookup-register name) 'trace-on)))
              ((eq? message 'reg-trace-off)
               (lambda (name) ((lookup-register name) 'trace-off)))
              ((eq? message 'set-breakpoint)
               add-breakpoint)
              ((eq? message 'proceed)
               (proceed))
              ((eq? message 'cancel-breakpoint)
               remove-breakpoint)
              ((eq? message 'cancel-all-breakpoints)
               (set! breakpoints '()))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (begin
                                (if (not (null? insts))
                                    (set-instruction-positions! insts next-inst)
                                    'ok)
                                (receive insts
                                         (cons (make-label-entry next-inst
                                                                 insts)
                                               labels)))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (list text '() '() '() '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-label inst)
  (cadr inst))

(define (instruction-offset inst)
  (caddr inst))

(define (instruction-execution-proc inst)
  (cadddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdddr inst) proc))

(define (set-instruction-label! inst label)
  (set-car! (cdr inst) label))

(define (set-instruction-offset! inst offset)
  (set-car! (cddr inst) offset))

(define (set-instruction-positions! insts label)
  (define (iter insts n)
    (if (null? insts)
        'done
        (let ((inst (car insts)))
          (if (not (null? (instruction-label inst)))
              'done
              (begin
                (set-instruction-label! inst label)
                (set-instruction-offset! inst n)
                (iter (cdr insts) (+ n 1)))))))
  (iter insts 1))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; ============================================================
;; 4.1:評価器が使う構文解析・環境・基本手続き
;; ============================================================

;; ===== 4.1.2:構文の判定・分解 =====
(define (self-evaluating? exp)
  (or (number? exp) (string? exp) (boolean? exp)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (null? (cdddr exp)) 'false (cadddr exp)))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (last-operand? ops) (null? (cdr ops)))
(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))

;; ===== 4.1.3:合成手続き・環境フレーム =====
(define (true? x) (not (eq? x #f)))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
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
  (cond ((= (length vars) (length vals))
         (cons (make-frame vars vals) base-env))
        ((< (length vars) (length vals))
         (error "Too many arguments supplied" vars vals))
        (else (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (null? env)
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
    (if (null? env)
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

;; ===== 4.1.4:基本手続き・大域環境 =====
(define primitive-procedures
  (list (list 'car car) (list 'cdr cdr) (list 'cons cons)
        (list 'null? null?) (list 'pair? pair?) (list 'list list)
        (list 'eq? eq?) (list 'equal? equal?)
        (list 'number? number?) (list 'symbol? symbol?)
        (list '+ +) (list '- -) (list '* *) (list '/ /)
        (list '= =) (list '< <) (list '> >)
        (list '<= <=) (list '>= >=) (list 'remainder remainder)
        (list 'abs abs) (list 'zero? zero?)
        (list 'length length) (list 'append append)
        (list 'cadr cadr) (list 'caddr caddr)
        (list 'not not) (list 'display display) (list 'newline newline)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))
(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

;; ============================================================
;; 5.4.4:評価器から呼び出す演算一覧
;; ============================================================

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
   (list 'if-alternative if-alternative)
   (list 'if-consequent if-consequent)
   (list 'true? true?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'define-variable! define-variable!)
   (list 'prompt-for-input prompt-for-input)
   (list 'read read)
   (list 'eof-object? eof-object?) ; REPL が EOF で停止するように追加
   (list 'get-global-environment get-global-environment)
   (list 'announce-output announce-output)
   (list 'user-print user-print)))

;; ============================================================
;; 4.1.2:condをifへ変換する補助手続き(本文の構文変換)
;; ============================================================

(define (false? x) (eq? x #f))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (car clause) 'else))
(define (sequence->exp seq)
  (cond ((null? seq) 'false)
        ((last-exp? seq) (first-exp seq))
        (else (cons 'begin seq))))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses)) (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cdr first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (car first)
                     (sequence->exp (cdr first))
                     (expand-clauses rest))))))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

;; ============================================================
;; 5.5.1〜5.5.4:Scheme式をレジスタ計算機の命令列に翻訳する
;; ============================================================

;; expは翻訳対象の式、targetは結果を置くレジスタ、linkageは処理後の行き先。
;; 式の種類を判定し、それぞれの翻訳手続きに処理を振り分ける。
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp) (compile-variable exp target linkage))
        ((assignment? exp) (compile-assignment exp target linkage))
        ((definition? exp) (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp) (compile-application exp target linkage))
        (else (error "Unknown expression type -- COMPILE" exp))))

;; 命令列を「先に必要なレジスタ」「書き換えるレジスタ」「命令本体」の3要素で表す。
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))
(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;; 次へ進むnext、呼び出し元へ戻るreturn、特定ラベルへの移動を命令列にする。
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next) (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))
;; 命令本体の後ろに制御移動を連結し、必要ならcontinueを保護する。
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue) instruction-sequence
              (compile-linkage linkage)))

;; 数値・文字列などを定数としてtargetへ格納する。
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,exp))))))
;; quoteされた式を評価せず、そのまま定数としてtargetへ格納する。
(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target
                                        (const ,(text-of-quotation exp)))))))
;; 環境envから変数の値を探してtargetへ格納する。
(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '(env) (list target)
                              `((assign ,target
                                        (op lookup-variable-value)
                                        (const ,exp)
                                        (reg env))))))

;; set!:右辺の式を評価し、その結果を既存の束縛へ代入する。
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op set-variable-value!) (const ,var)
                  (reg val) (reg env))
         (assign ,target (const ok))))))))

;; define:右辺の式を評価し、その結果を環境へ登録する。
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op define-variable!) (const ,var)
                  (reg val) (reg env))
         (assign ,target (const ok))))))))

(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ label-counter 1))
  label-counter)
;; 分岐先ラベルに通し番号を付け、同じ名前の衝突を避ける。
(define (make-label name)
  (string->symbol (string-append (symbol->string name)
                                 (number->string (new-label-number)))))

;; 条件式・真の場合・偽の場合を翻訳し、分岐命令で接続する。
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code (compile (if-consequent exp)
                             target consequent-linkage))
            (a-code (compile (if-alternative exp)
                             target linkage)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;; beginやlambda本体の式列を順番に翻訳する。
;; 最後の式だけは外側から指定されたlinkageを引き継ぐ。
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq)
                                    target linkage))))

;; 翻訳済み手続きは、命令の入り口と定義時の環境を保持する。
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

;; lambdaは本体を直ちに実行せず、入り口ラベルを持つ手続き値を作る。
(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target (op make-compiled-procedure)
                    (label ,proc-entry) (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

;; 呼び出し時に、保存済み環境へ引数を束縛して本体を評価する命令を作る。
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment)
                (const ,formals) (reg argl) (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

;; 手続き適用:演算子の値、引数の値を順に用意して呼び出す。
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

;; 引数の翻訳結果を、実行時にarglレジスタへ集める命令列へ変換する。
;; 本文の方式に合わせ、後ろの引数から評価してconsで前につなぐ。
(define (construct-arglist operand-codes)
  (let ((reversed-codes (reverse operand-codes)))
    (if (null? reversed-codes)
        (make-instruction-sequence
         '() '(argl) '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car reversed-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr reversed-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args (cdr reversed-codes))))))))

;; 残りの引数の評価コードを連結し、既にあるarglを必要に応じて保存する。
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

;; 5.5.3:手続き呼び出しの種類によって、生成するコードを分ける。
;; 本文の基本形では、基本手続きか翻訳済み手続きのどちらかを扱う。
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl) (list target)
           `((assign ,target (op apply-primitive-procedure)
                     (reg proc) (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        (else
         (error "return linkage, target not val -- COMPILE" target))))

(define all-regs '(env proc val argl continue))

;; ============================================================
;; 5.5.4:命令列の結合とレジスタの保存が必要かの判定
;; ============================================================

(define (registers-needed seq)
  (if (symbol? seq) '() (car seq)))
(define (registers-modified seq)
  (if (symbol? seq) '() (cadr seq)))
(define (statements seq)
  (if (symbol? seq) (list seq) (caddr seq)))
(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;; 命令列を連結し、レジスタの必要・変更情報も更新する。
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list remaining)
    (if (null? remaining)
        (empty-instruction-sequence)
        (append-2-sequences (car remaining)
                            (append-seq-list (cdr remaining)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

;; seq2が必要とするレジスタをseq1が書き換える場合だけsave/restoreする。
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving
             (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg) (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

;; ラムダの本体コードなどを、外側のレジスタ情報には含めず末尾へ置く。
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))
;; 分岐先のように片方だけが実行される命令列を結合する。
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;; 翻訳結果を表示するだけの学習用補助関数。
(define (display-compiled-code expression)
  (for-each (lambda (statement)
              (display statement)
              (newline))
            (statements (compile expression 'val 'return)))
  'done)


;; 翻訳済みコードが使う演算を評価器の演算表へ登録する。
(set! eceval-operations
      (append eceval-operations
              (list
               (list 'false? false?)
               (list 'cond? cond?)
               (list 'cond->if cond->if)
               (list 'make-compiled-procedure make-compiled-procedure)
               (list 'compiled-procedure? compiled-procedure?)
               (list 'compiled-procedure-entry compiled-procedure-entry)
               (list 'compiled-procedure-env compiled-procedure-env)
               (list 'cons cons)
               (list 'list list))))

(define eceval
  (make-machine
   '(exp
      ; 現在評価しようとしている「1つの式」
      ; 例: '(+ 1 2), '+, '1
      ; eval-dispatchはこのレジスタを見て評価方法を決める

      env
      ; expを評価するための環境(変数名と値の対応を持つ環境フレームの連鎖)
      ; 例: x=10という束縛を持つ環境
      ; 変数xを評価するとき、この環境から値10を探す

      val
      ; 直前に評価した式の結果(Schemeの任意の値)
      ; 例: (+ 1 2) の評価結果なら3
      ; 部分式の評価結果もここに入り、次の処理に渡される

      proc
      ; 現在適用しようとしている手続き
      ; 例: (+ 1 2) なら、変数+を評価して得た加算手続き
      ; apply-dispatchがこの手続きをarglの引数に適用する

      argl
      ; 手続きに渡す、評価済みの実引数の値のリスト
      ; 例: (+ x (* 2 3)) でx=10なら、最終的に(10 6)
      ; 引数の「式」ではなく、評価した「値」を保持する

      continue
      ; 現在の評価が終了した後に実行を再開する場所
      ; 例: ev-appl-did-operator(演算子の評価が終わった後の処理)
      ; 関数呼び出しにおける戻りアドレスの役割を果たす

      unev
      ; 未処理の式の列(オペランド、手続き本体など)を保持する
      ; 例: (+ x (* 2 3)) の引数部分なら '(x (* 2 3))

      silent?)
   ; 対話表示を省略して結果だけ取得する補助機能用のフラグ
   eceval-operations
    '(;; 最初の分岐条件に使うflagは、起動用手続きであらかじめ設定する。
      (branch (label external-entry))
      (test (op true?) (reg silent?))
      (branch (label internal-eval-entry))

      read-eval-print-loop
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (test (op eof-object?) (reg exp))
      (branch (label evaluator-done))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label eval-dispatch))

      ;; 5.5.7本文:翻訳して生成した命令列を実行する入り口。
      external-entry
      (perform (op initialize-stack))
      (assign env (op get-global-environment))
      (test (op true?) (reg silent?))
      (branch (label external-silent-entry))
      (assign continue (label print-result))
      (goto (reg val))

      ;; 補助手続き用:対話ループへ戻らず実行結果だけを返す。
      external-silent-entry
      (assign continue (label evaluator-done))
      (goto (reg val))

      internal-eval-entry
      (perform (op initialize-stack))
      (assign env (op get-global-environment))
      (assign continue (label evaluator-done))
      (goto (label eval-dispatch))

      print-result
      (perform (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))

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
      (test (op cond?) (reg exp))
      (branch (label ev-cond))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))

      ev-cond
      (assign exp (op cond->if) (reg exp))
      (goto (label eval-dispatch))

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
      (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
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
      (restore unev)
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val))
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
      (test (op compiled-procedure?) (reg proc))
      (branch (label compiled-apply))
      (goto (label unknown-procedure-type))

      primitive-apply
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (restore continue)
      (goto (reg continue))

      compiled-apply
      (restore continue)
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))

      compound-apply
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
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
      (goto (label eval-dispatch))

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
      (save unev)
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))

      ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

      ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))

      ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op define-variable!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

      unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))

      unknown-procedure-type
      (restore continue)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))

      signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))

      evaluator-done)))


;; ============================================================
;; 5.5.7:翻訳したコードと評価器を接続する
;; ============================================================

(define (reset-global-environment!)
  (set! the-global-environment (setup-environment))
  'done)

;; 式を翻訳して実行し、対話ループに入らず結果を取り出すための補助関数。
;; compile-and-goとは異なり、呼び出し間で大域環境を保持する。
(define (compile-and-eval expression)
  (let ((instructions
         (assemble (statements (compile expression 'val 'return))
                   eceval)))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'silent? #t)
    (set-register-contents! eceval 'flag #t)
    (start eceval)
    (get-register-contents eceval 'val)))

;; 翻訳せず、5.4節の評価器で式を評価する補助関数。
(define (interpret-and-eval expression)
  (set-register-contents! eceval 'exp expression)
  (set-register-contents! eceval 'silent? #t)
  (set-register-contents! eceval 'flag #f)
  (start eceval)
  (get-register-contents eceval 'val))

;; 5.5.7本文のインターフェース。式を翻訳・実行した後、対話ループへ進む。
;; 教科書どおり、呼び出し時に大域環境を初期化する。
(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements (compile expression 'val 'return))
                   eceval)))
    (reset-global-environment!)
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'silent? #f)
    (set-register-contents! eceval 'flag #t)
    (start eceval)))

;; 通常の積極制御評価器を、大域環境を初期化して起動する。
(define (start-eceval)
  (reset-global-environment!)
  (set-register-contents! eceval 'silent? #f)
  (set-register-contents! eceval 'flag #f)
  (start eceval))

(define (stack-statistics)
  ((eceval 'stack) 'statistics))
(define (print-stack-statistics)
  ((eceval 'stack) 'print-statistics))

;; 基本機能が動くかを確かめるための簡単な確認用関数。
;; 演習問題の解答・測定・分析は含めない。
(define (check-equal label expression expected evaluator)
  (let ((actual (evaluator expression)))
    (if (equal? actual expected)
        (begin (display "PASS: ") (display label) (newline))
        (error "FAILED" label actual expected))))

(define (run-smoke-tests)
  (reset-global-environment!)
  (check-equal 'number '42 42 compile-and-eval)
  (check-equal 'quote '(quote (a b)) '(a b) compile-and-eval)
  (check-equal 'arithmetic '(+ 1 (* 2 3)) 7 compile-and-eval)
  (check-equal 'no-arguments '(list) '() compile-and-eval)
  (check-equal 'if-true '(if true 17 99) 17 compile-and-eval)
  (check-equal 'if-false '(if false 17 99) 99 compile-and-eval)
  (check-equal 'cond '(cond ((= 1 2) 20) (else 30)) 30 compile-and-eval)
  (check-equal 'closure '(((lambda (x) (lambda (y) (+ x y))) 7) 5)
               12 compile-and-eval)
  (check-equal 'define '(define (square x) (* x x))
               'ok compile-and-eval)
  (check-equal 'compiled-call '(square 5) 25 compile-and-eval)
  (check-equal 'interpreted-call '(square 6) 36 interpret-and-eval)
  (check-equal 'set-and-begin
               '(begin (define a 2) (set! a (+ a 3)) a)
               5 compile-and-eval)
  (display "基本動作の確認が完了しました。")
  (newline)
  'done)

;; ============================================================
;; 実行例(DrRacketで読み込んだ後の対話ウィンドウから使用)
;; ============================================================
;; (run-smoke-tests)
;; (display-compiled-code '(+ 1 (* 2 3)))
;; (compile-and-eval '(define (square x) (* x x)))  ; 結果:ok
;; (compile-and-eval '(square 5))                   ; 結果:25
;; (print-stack-statistics)                        ; スタック利用状況
;; (compile-and-go '(define (square x) (* x x)))    ; 翻訳後に対話起動
;; (start-eceval)                                  ; 5.4節の評価器を起動
;;
;; 5.5.6の文面アドレスによる最適化は未実装。
;; 変数の参照は本文の基本形と同様、lookup-variable-valueで行う。
