#lang sicp
; 方針: let を lambda 適用への構文変換で扱う。

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond
    ; 自己評価式（例: 数値）に対してはそれ自身を返す
    ((self-evaluating? exp) exp)
    ; 値を得るため環境から変数を探す
    ((variable? exp) (lookup-variable-value exp env))
    ; クォートされた式を返す
    ((quoted? exp) (text-of-quotation exp))
    ; 変数の代入・定義の場合、、対応づける新しい値を計算するためevalを再帰的に呼ぶ
    ; 束縛を修正・作成して環境を修正
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ; if 式は、述語が真なら帰結式を評価し、そうでなければ代替式を評価するよう、要素式の特別な処理を必要とする
    ((if? exp) (eval-if exp env))
    ; lambda 式は lambda 式が指定したパラメタと本体を、評価の環境とともに詰め合わせ、作用可能な手続きへと変換する必要がある
    ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
    ; begin 式は、要素式の並びを現れる順に評価する必要がある
    ((begin? exp)
     (eval-sequence (begin-actions exp) env))
    ; cond による場合分けは if 式の入れ子に変換してから評価する
    ((cond? exp) (eval (cond->if exp) env))
    ; let は導出された式として扱う
    ((let? exp) (eval (let->combination exp) env))
    ((application? exp)
     (my-apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    ; 対応できない場合
    (else
     (error "Unknown expression type -- EVAL" exp))))


; applyは二つの引数, 手続きと, 手続きを作用させる引数のリストをとる.
; applyは手続きを二つに場合分けする
; 基本演算を作用させるのに, apply-primitive-procedureを呼び出す
; 合成手続きは手続きの本体を構成する式を順に評価して作用させる.
; 合成手続きの本体の評価における環境は, 手続きによって持ち込まれた基本の環境を, 手続きのパラメタと手続きを作用させようとする引数を束縛するフレームへ拡張して構成する.
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


; eval-if は if 式の述語部を与えられた環境で評価する
; 結果が真なら、eval-if は帰結部を評価し、そうでなければ代替部を評価する
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


; eval-sequence は、apply が手続き本体中の並びの、また eval が begin 式中の要素式の評価に使う。
; 引数として式の並びと環境をとり、現れる順に式を評価する。
; 返す値は最後の式の値。
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


; eval-assignment は代入する値を見つけるために eval を呼び出し、変数と結果の値を set-variable-value! に渡し、指示した環境に設定する。
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)


; eval-definition は変数と同様に扱える
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


; 自己評価式は数と文字列だけである
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
; cond 式を if 式に変換する cond->if に使う if 式の構成式
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


; begin は式の並びを単一の式に。
; begin 式から実際の並びが取れるように begin の構文演算と、並びの最初の式と残りの式を繰り返す選択肢を用意する。
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))
; (cond->ifが使う) 構成子 sequence->exp も。
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


; 手続き作用は任意の合成式。
; 式の car が演算子、cdr が非演算子のリストである。
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


; cond 式の要素を取り出す構文手続きと、cond 式を if 式に変換する手続き cond->if。
; 場合分けは cond で始まり、述語と行動の節のリストを持つ。
; 述語が記号 else の時、節は else 節である。
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

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
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; let 式は導出された式
(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (binding-variable binding) (car binding))

(define (binding-value binding) (cadr binding))

(define (let-variables exp)
  (map binding-variable (let-bindings exp)))

(define (let-values exp)
  (map binding-value (let-bindings exp)))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp)
                     (let-body exp))
        (let-values exp)))
            

; 条件式では明白に false であるオブジェクト以外は true
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


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
        ; (基本手続きが続く)
        ))

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

(define the-global-environment (setup-environment))


; apply が手続き primitive-procedure? と apply-primitive-procedure を使ってオブジェクトを識別でき、作用できれば、基本手続きオブジェクトをどう表現するかは。
; 記号 primitive で始まり基本手続きを実指示ている基盤 Lisp での手続きを含むリストとして基本手続きを表現するよう選んだ。
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

; 基本手続きを作用させるには、基盤 Lisp 使い、実装手続きを引数に作用させるだけである
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; 基盤 Lisp システムの読み込み-評価-印字ループをモデル化する駆動ループを用意する
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eof-object? input)
        'done
        (let ((output (eval input the-global-environment)))
          (announce-output output-prompt)
          (user-print output)
          (driver-loop)))))

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


; 問題4.6 確認用
(define (test-4.6)
  (eval '(let ((x 'a) (y 'b))
           (cons x (cons y '())))
        the-global-environment))




