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
; ---

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
#|
1. assoc

; (assoc 'a '((a 1) (b 2)))
; -> (a 1)
; (cond ((list 1 2) => cadr))
; -> 2


(cond
  ((null? '()) 'null)
  ((list? '()) 'list)
  (else 'else))


2. (expand-clauses clauses) で (display clauses) してみる
(cond
  ((null? '()) 'null)
  ((list? '(a)) 'list)
  (else 'else))
--first
((null? '()) 'null)
--rest
(((list? '(a)) 'list) (else 'else))
--first
((list? '(a)) 'list)
--rest
((else 'else))
--first
(else 'else)
--rest
()

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))
--first
((assoc 'b '((a 1) (b 2))) => cadr)
--rest
((else false))
--first
(else false)
--rest
()

3要素かつ2要素目が"=>"

動作検証
(cond ('(1 2) => car))
(cond ('(1 2) => cdr))
(cond (false => car))
|#

(define (cond-arrow-clause? clause)
  (and (> (length clause) 2); 2要素より大きい
       (eq? (cadr clause) '=>))) ; 2番目が=>

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; elseなし
      (let ((first (car clauses))
            (rest (cdr clauses)))
        #|
        (display '--first)
        (newline)
        (display first)
        (newline)
        (display '--rest)
        (newline)
        (display rest)
        (newline)
        |#
        (cond
          ((cond-else-clause? first)
           (if (null? rest)
               (sequence->exp (cond-actions first))
               (error "ELSE clause isn't last --COND->IF"
                      clauses)))
          ((cond-arrow-clause? first) ; (test => recipient)
           ; test: (car first)
           ; recipient: (caddr first)

           #| 最初にかいたやつ。((caddr first) test) が即時で実行されてエラーになった
           (let ((test (car first)))
             (if test
                 ((caddr first) test)
                 (expand-clauses rest))))
          |#

           ; (if (car first) ((caddr first) (car first)) (expand-clauses rest)) の形式をつくる
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

(define (make-frame variables values)
  (cons variables values))

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

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!!" var)
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
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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
#|
(let ((x 42)) x)
((lambda (x) x) 42)

let:
(let ({var} {exp}) {body})

lambda:
((lambda ({var})) {body}) {exp})

名前付きlet:
(let {var} {bindings} {body})

名前つきletは2番目がsymbol


(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

(fib 5)

↓ lambdaへ

(define (fib-2 n)
  ((lambda ()
     (define fib-iter (lambda (a b count)
                        (if (= count 0)
                            b
                            (fib-iter (+ a b) a (- count 1)))))
     (fib-iter 1 0 n))))

(fib-2 5)

|#

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

(driver-loop)




