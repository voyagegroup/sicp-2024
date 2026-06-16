#lang sicp

; get/put(3.3.3より)
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
  

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


; --- 4.1.1

; evalは引数として式と環境をとる. 式を分類して評価を振り分ける. evalは評価すべき式の構文の型の場合分けの構造とする. 
; --- 4.3
#|
(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))


    ((get 'eval (car exp))
     ((get 'eval (car exp)) exp env))

    #|
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
        |#
    ((application? exp)
     (my-apply (eval (operator exp) env)
               (list-of-values (operands exp) env)))

    (else
     (error "Unknown expression type -- EVAL" exp))))
|#
; ---

; --- 4.1.7
(define (eval exp env)
  ((analyze exp) env))


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
        ((let? exp)(analyze-let exp))  ; 4.22
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


(define (analyze-self-evaluating exp)
  (lambda (env) exp))


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (pproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))


(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

; --- 4.1.7

; applyは二つの引数, 手続きと, 手続きを作用させる引数のリストをとる. applyは手続きを二つに場合分けする: 基本演算を作用させるのに, apply-primitive-procedureを呼び出す
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; list-of-valuesは引数として組合せの被演算子をとり, 各被演算子を評価し, 対応する値のリストを返す
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; eval-ifはif式の述語部を与えられた環境で評価する. 結果が真なら, eval-ifは帰結部を評価し, そうでなければ代替部を評価する
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; eval-sequenceは, applyが手続き本体中の要素式の並びの評価に, またevalがbegin式中の要素式の並びの評価に使う. 引数として式の並びと環境をとり, 現れる順に式を評価する. 返す値は最後の式の値である.
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; 次の手続きは変数への代入を扱う. 代入する値を見つけるためにevalを呼び出し, 変数と結果の値をset-variable-value!に渡し, 指示した環境に設定させる.
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; -- 4.1.2

; •自己評価式は数と文字列だけである:
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; •変数は記号で表現する:
(define (variable? exp) (symbol? exp))

; •クォート式は(quote ⟨text-of-quotation⟩)の形である
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; quoted?は, 指示した記号で始るリストを識別する手続きtagged-list?を使って定義する:
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; •代入は(set! ⟨var⟩  ⟨value⟩)の形である:
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

#|
•定義は
(define ⟨var⟩ ⟨value⟩)
または
(define (⟨var⟩ ⟨parameter1⟩ ... ⟨parametern⟩)
  ⟨body⟩)
の形である. 後の形(標準手続き定義)は
(define ⟨var⟩
  (lambda (⟨parameter1⟩ ... ⟨parametern⟩)
    ⟨body⟩))
の 構文シュガーである. 
|#

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; 仮パラメタ
                   (cddr exp)))) ; 本体

; •lambda式は記号lambdaで始るリストである:
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

; また上のdefinition-valueが使うlambda式の構成子も用意する:
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; •条件式はifで始り, 述語と帰結部と(場合により)代替部を持つ. 式に代替部がなければ, 代替部としてfalseを置く
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; またcond式をif式に変換するcond->ifに使うif式の構成子も用意する:
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; •beginは式の並びを単一の式に包み込む. begin式から実際の並びが取れるようにbegin式の構文演算と, 並びの最初の式と残りの式を返す選択子を用意する.
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

; (cond->ifが使う)構成子sequence->expも用意する. これは必要ならbeginを使い, 並びを単一の式に変換する:
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; •手続き作用は上の式の形のいずれでもない任意の合成式である. 式のcarが演算子, cdrが被演算子のリストである:
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

           
; cond式の要素を取り出す構文手続きと, cond式をif式に変換する手続きcond->ifを用意する. 場合分けはcondで始り, 述語と行動の節のリストを持つ. 述語が記号elseの時, 節はelse節である.
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))


; --- 4.5
(define (cond-arrow-clause? clause)
  (and (> (length clause) 2); 2要素より大きい
       (eq? (cadr clause) '=>))) ; 2番目が=>

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; elseなし
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond
          ((cond-else-clause? first)
           (if (null? rest)
               (sequence->exp (cond-actions first))
               (error "ELSE clause isn't last --COND->IF"
                      clauses)))
          ((cond-arrow-clause? first) ; (test => recipient)
           (list 'if
                 (car first)
                 (list (caddr first) (car first))
                 (expand-clauses rest)))
           
          (else (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

; --- 4.5


; 4.1.3

; 述語のテスト

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

; 手続きの表現
#|
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env)) ; 4.16 c scan-out-definesを噛ませる。こっちに組み込めば、手続きの作成時だけに実行されることになるので、O(1) となる。
|#

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

; 環境に対する操作
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

; --- 4.11 frameの操作
(define (make-frame variables values)
  ; (cons variables values))
  (map cons variables values))

(define (frame-variables frame)

  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (add-binding-to-frame! var val frame env)
  (set-car! env (cons (cons var val) frame)))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


; --- 4.12
(define (scan-frame var frame found-action not-found-action)
  (define (scan bindings)
    (cond ((null? bindings)
           (not-found-action var))
          ((eq? var (caar bindings)) ; 変数名をチェック
           (found-action (car bindings))); 束縛の全体を渡す
          (else (scan (cdr bindings)))))
  (scan frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    #|
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
|#
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        #|
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  |#
        (scan-frame 
         var
         (first-frame env)
         ; (lambda (binding) (cdr binding))  ; 見つかったら値を返す
         (lambda (binding) ; 4.16 a. *unsigned* をエラーにする
           (if (eq? (cdr binding) '*unassigned*)
               (error 'unassigned var)
               (cdr binding)))
         (lambda (var) (env-loop (enclosing-environment env)))))); 見つからなかったら次の環境
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!!" var)
        (scan-frame 
         var (first-frame env)
         (lambda (binding) (set-cdr! binding val) 'ok); 値を更新
         (lambda (var) (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (scan-frame 
   var (first-frame env)
   (lambda (binding) (set-cdr! binding val) 'ok) ; 更新
   (lambda (var) (add-binding-to-frame! var val (first-frame env) env) 'ok))) ; 新規


; --- 4.12

; 4.1.4

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '< <)
        (list '> >)
        (list 'display display)
        (list 'map map)
        ; 基本手続きが続く
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))


(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))





(define (apply-primitive-procedure proc args)
  (display 'proc:)
  (display proc)(newline)
  (display 'args)
  (display args)(newline)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)

#|
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
|#

; 4.4.4
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

; 4.4.4
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


; --- 3.5.1 ストリーム
(define (stream-car stream) (car stream))


(define (stream-cdr stream) (force (cdr stream)))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

; ---


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)))
      (display object)))



; --- 4.3
(put 'eval 'quote
     (lambda (exp env) (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'if eval-if)
(put 'eval 'define eval-definition)

(put 'eval 'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
                       (lambda-body exp)
                       env)))

(put 'eval 'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))

(put 'eval 'cond
     (lambda (exp env)
       (eval (cond->if exp) env)))
; ---

; --- 4.4: orとandの実装
(define (last? list)
  (null? (cdr list)))

; and: 式を左から右へ評価する. ある式が偽に評価されたら偽を返す; 残りの式は評価しない. すべての式が真に評価されたら, 最後の式の値を返す. 式が一つもなければ真を返す.
(define (eval-and exp env)
  (define (iter operands)
    (cond ((null? operands) true) ; 1つもない
          ((last? operands) (eval (car operands) env)) ; 最後なのでそのまま返す
          (else
           (let ((result (eval (car operands) env)))
             (if (false? result)
                 false ; 短絡評価
                 (iter (cdr operands))))))) ; trueの場合再帰
  (iter (cdr exp)))
(put 'eval 'and eval-and)


; or: 式を左から右へ評価する. ある式が真の値に評価されたらその値を返す; 残りの式は評価しない. すべての式が偽に評価されるか, 式が一つもなければ偽を返す.
(define (eval-or exp env)
  (define (iter operands)
    (cond ((null? operands) false) ; 1つもない
          (else
           (let ((result (eval (car operands) env)))
             (if (true? result)
                 true ; 短絡評価
                 (iter (cdr operands))))))) ; andとは違い、すべてがfalseの場合はnull?になるので、last?はいらない
  (iter (cdr exp)))
(put 'eval 'or eval-or)
; --- 4.4


; --- 4.6 let->combinationの実装
; --- 4.7 名前つきlet
; 2番目がsymbolの場合はなまえ付きのlet
(define (named-let? exp)
  (symbol? (cadr exp)))

(define (let->combination exp)
  (if (named-let? exp)
      ; 名前つきletの場合
      (let ((var (cadr exp))
            (bindings (caddr exp))
            (body (cdddr exp)))
        (list (list 'lambda '() ; 引数なしのlambda
                    (list 'define var ; varでdefine
                          (make-lambda (map car bindings) body))
                    (cons var (map cadr bindings)))))

      ; 普通のlet
      (let ((bindings (cadr exp))
            (body (cddr exp)))
        (cons (make-lambda (map car bindings) body)
              (map cadr bindings)))))

(put 'eval 'let
     (lambda (exp env)
       (eval (let->combination exp) env)))


; --- 4.6

; --- 4.7 let*
(define (let*->nested-lets exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (if (null? bindings)
        (sequence->exp body) ; 最後
        (list 'let
              (list (car bindings))
              (let*->nested-lets (cons 'let* (cons (cdr bindings) body))))
        )
    ))

(put 'eval 'let*
     (lambda (exp env)
       (eval (let*->nested-lets exp) env)))

; --- 4.7


; --- 4.9 for

(define (for->let exp)
  (let ((var-init (cadr exp)) ; (i 0)
        (condition (caddr exp)) ; (< i 100)  
        (update (cadddr exp)) ; (+ i 1)
        (body (cddddr exp))) ; (display i) 
    ; 名前つきletに変換
    (list 'let 'for-loop 
          (list var-init)
          (list 'if condition
                (cons 'begin 
                      (append body 
                              (list (list 'for-loop update))))
                ''done))))

(define (eval-for exp env)
  (eval (for->let exp) env))

(put 'eval 'for eval-for) 

; --- 4.9

; --- 4.10
(define (dainyu-variable exp)
  (cadr exp))

(define (dainyu-value exp)  
  (caddr exp))

(define (eval-dainyu exp env)
  (set-variable-value! (dainyu-variable exp)
                       (eval (dainyu-value exp) env)
                       env)
  'ok)
(put 'eval 'dainyu eval-dainyu)

; ---

; -- 4.13 unbind!
#|
現在のフレームからだけ削除するべきである。
(define x 10) ; 大域環境
(let ((x 11)) ; ここの環境のxが消されるべきである
  (unbind!x))

大域環境を削除すると、大域環境を壊してしまう
|#

(define (unbind? exp) (tagged-list? exp 'unbind!))
(define (unbind-variable exp) (cadr exp))

(define (eval-unbind exp env)
  (let ((var (unbind-variable exp))
        (frame (first-frame env)))
    (set-car! env (remove-binding var frame))
    'ok))

(put 'eval 'unbind! eval-unbind)

(define (remove-binding var bindings)
  (cond ((null? bindings) '()) ; 見つからない
        ((eq? var (caar bindings))  ; 見つかった
         (cdr bindings)) ; この束縛を除去
        (else 
         (cons (car bindings)   ; この束縛は保持
               (remove-binding var (cdr bindings))))))  ; 再帰的に探索

; ---

; --- 4.17 b 内部定義をなくす
(define (make-let bindings body)
  (cons 'let (cons bindings body)))


(define (make-set! var val)
  (list 'set! var val))

(define (scan-out-defines body)
  (display body)(newline)

  ; definesの定義とそれ以外とをわける君
  ; defines = ((define a 1) (define b 2))
  ; non-defines = ((+ a b))
  (define (collect body)
    (define (iter exps defines non-defines)
      (cond ((null? exps)
             (list defines non-defines))
            ((definition? (car exps)) ; definition に追加
             (iter (cdr exps) (cons (car exps) defines) non-defines))
            (else ; non-definition に追加
             (iter (cdr exps) defines (cons (car exps) non-defines)))))
    (iter body '() '()))

  ; 変換処理: definesをset!をつかった形式に変換する
  (define (translate vars vals non-defines)
    (let* ((bindings (map (lambda (var) (list var ''*unassigned*)) vars))
           (set-exprs (map make-set! vars vals))
           (let-body (append set-exprs non-defines)))

      (list (make-let bindings let-body))))


  (let* ((result (collect body))
         (defines (car result))
         (non-defines (cadr result)))

    (if (null? defines) 
        body ; definesがなかったら何もしない
        (translate (map definition-variable defines) (map definition-value defines) non-defines)))) ; あったら、変換処理

; --- 4.16 b ---

; --- 4.20 letrec ---
#|
(letrec ((v1 e1)
         (v2 e2))
  body1
  body2)

↓

(let ((v1 '*unassigned*)
      (v2 '*unassigned*))
  (set! v1 e1)
  (set! v2 e2)
  body1
  body2)

--

(letrec ((x 1)
         (y 2))
  (+ x y))

vars(x y)
exps(1 2)
body((+ x y))


|#

(put 'eval 'letrec
     (lambda (exp env)
       (eval (letrec->let exp) env) ))

(define (letrec->let exp)
  (let ((vars (map car (cadr exp)))
        (exps (map cadr (cadr exp)))
        (body (cddr exp)))
    (display 'vars)
    (display vars)(newline)
    (display 'exps)
    (display exps) (newline)
    (display 'body)
    (display body)(newline)

    (let ((new-bindings
           (map (lambda (v)
                  (list v ''*unassigned*))
                vars))
          (set-exprs (map make-set! vars exps)))
      (display new-bindings)

      (make-let new-bindings (append set-exprs body)))))
        


; --- 4.20 ---


; --- 4.22 analyze let ---
(define (let? exp) (tagged-list? exp 'let))

(define (analyze-let exp)
  (analyze (let->combination exp)))


; 4.4.4
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

; 4.4.4 単純質問

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

; 4.4.4 合成質問
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

; 4.4.4 フィルタ
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define user-initial-environment the-global-environment)

(define (execute exp)
  (my-apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'and 'qeval conjoin)
(put 'or 'qeval disjoin)
(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)
(put 'always-true 'qeval always-true)

; 4.4.4.3

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

; 4.4.4.4

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))


(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))  ; ***
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                      ; ***
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)     ; ***
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

; 4.4.4.5
(define THE-ASSERTIONS the-empty-stream)


(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)


(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))


(define (use-index? pat)
  (constant-symbol? (car pat)))

; 4.4.4.6
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))


(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))


(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))


(define (singleton-stream x)
  (cons-stream x the-empty-stream))

; 4.4.4.7
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))


(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))


(define (contract-question-mark variable)
  (string->symbol
   (string-append "?" 
     (if (number? (cadr variable))
         (string-append (symbol->string (caddr variable))
                        "-"
                        (number->string (cadr variable)))
         (symbol->string (cadr variable))))))

; 4.4.4.8
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))



(query-driver-loop)

; (driver-loop)


#| テストデータの挿入
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))

(assert! (can-do-job (computer programmer)
                     (computer programmer trainee)))

(assert! (can-do-job (administration secretary)
                     (administration big wheel)))


(assert! (rule (same ?x ?x)))


(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))
|#















