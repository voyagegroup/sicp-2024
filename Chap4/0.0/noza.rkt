#lang sicp

(define (eval exp env)
  (cond
    ; 自己評価式（例: 数値）に対してはそれ自身を返す
    ((self-evaluationg? exp) exp)
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
    ((lamgda? exp)
     (make-procedure (lambda-parameters exp)
                     (lambda-body)
                     env))
    ; begin 式は、要素式の並びを現れる順に評価する必要がある
    ((begin? exp)
     (evale-sequence (begin-actions exp) env))
    ; cond による場合分けは if 式の入れ子に変換してから評価する
    ((cond? exp) (eval (conf->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
    ; 対応できない場合
    (else
     (error "Unknown expression type -- EVAL" exp))))


; applyは二つの引数, 手続きと, 手続きを作用させる引数のリストをとる.
; applyは手続きを二つに場合分けする
; 基本演算を作用させるのに, apply-primitive-procedureを呼び出す
; 合成手続きは手続きの本体を構成する式を順に評価して作用させる.
; 合成手続きの本体の評価における環境は, 手続きによって持ち込まれた基本の環境を, 手続きのパラメタと手続きを作用させようとする引数を束縛するフレームへ拡張して構成する. 
(define (apply procedure arguments)
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
(define (self-evaluationg? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


; 変数は記号で表現する
(define (variable? exp) (symble? exp))


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


; 定義は (define <var> <value>) または (define (<var> <parameter1> ... <parameterN>O <body>) の形である
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (difinition-value exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (make-lambda (cdadr exp) ; 仮パラメタ
                   (cddr exp)))) ; 本体


; lambda 式は記号 lambda で始まるリストである
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (addr exp))

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

(define (last-exp? seq) (null? cdr seq))

(define (first-exp seq) (car seq))
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

(define (no-operands ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


; cond 式の要素を取り出す構文手続きと、cond 式を if 式に変換する手続き cond->if。
; 場合分けは cond で始まり、述語と行動の節のリストを持つ。
; 述語が記号 else の時、節は else 節である。
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clase) 'else))

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
            
