#lang sicp

(#%provide setup-environment
           prompt-for-input
           self-evaluating?
           variable?
           quoted?
           assignment?
           definition?
           if?
           lambda?
           begin?
           application?
           lookup-variable-value
           text-of-quotation
           lambda-parameters
           lambda-body
           make-procedure
           operands
           operator
           empty-arglist
           no-operands?
           first-operand
           last-operand?
           adjoin-arg
           rest-operands
           primitive-procedure?
           compound-procedure?
           apply-primitive-procedure
           procedure-parameters
           procedure-environment
           extend-environment
           procedure-body
           begin-actions
           first-exp
           last-exp?
           rest-exps
           if-predicate
           true?
           if-alternative
           if-consequent
           assignment-variable
           assignment-value
           set-variable-value!
           definition-variable
           definition-value
           define-variable!
           announce-output
           user-print)

; 第4章の評価器で使った構文・環境・手続きの操作。
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


; 変数は記号で表現する
(define (variable? exp) (symbol? exp))


; クォート式は (quote <text-of-quotation>) の形である。
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))
; tagged-list? は、指示した記号で始まるリストを識別する
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


; 代入は (set! <var> <value>) の形である
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


; 定義は (define <var> <value>) または (define (<var> <parameter1> ... <parameterN> <body>) の形である
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; 仮パラメタ
                   (cddr exp)))) ; 本体


; lambda 式は記号 lambda で始まるリストである
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; 条件式は if 始まり、述語と帰結部と代替部を持つ。式に代替部がなければ代替部に false をおく。
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; begin は式の並びを単一の式に。
; begin 式から実際の並びが取れるように begin の構文演算と、並びの最初の式と残りの式を繰り返す選択肢を用意する。
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

; 手続き作用は任意の合成式。
; 式の car が演算子、cdr が非演算子のリストである。
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; 条件式では明白に false であるオブジェクト以外は true
(define (true? x)
  (not (eq? x false)))

; 合成手続きはパラメタ、手続き本体および環境から、構成子 make-procedure を使って構成する
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))


(define (procedure-environment p) (cadddr p))


; 環境に対する操作
; 環境をフレームのリストとして表現する
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())
; 環境の各フレームはそのリストの対: そのフレームで束縛されている変数のリストと、対応づけられている値のリスト
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; 変数を値に対応づける新しいフレームで環境を拡張するには、変数のリストと値のリストからなるフレームを作り、これを環境に接続する。変数の個数が値の個数に一致しなければエラーとする。
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
; 環境の中で変数を探すには、最初のフレームで変数のリストを走査する。探している変数が見つかれば、値のリストの対応する要素を。
; 現在のフレームに変数が見つからなければ外側の環境を探し、これを続ける。
; 空の環境に達したら「未束縛変数」エラーを出す。
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
; 指定された環境の中で、変数を新しい値に設定するには、lookup-variable-value のように変数を操作し、それが見つかれば対応する値を更新する
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; setup-environment は基本手続きの名前と実装手続きをリストからとる
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

; 大域環境を設定する。
; 評価する式で変数として使えるように、記号 true と false の束縛もある。
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

; 基本手続きを作用させるには、基盤 Lisp 使い、実装手続きを引数に作用させるだけである
(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     ""))
      (display object)))

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))
