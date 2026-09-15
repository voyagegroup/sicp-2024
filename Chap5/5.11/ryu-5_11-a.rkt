#lang sicp

; 5.2.1 計算機モデル
; 与えられたレジスタ, 演算および制御器を持つ計算機のモデルを構成し, それを返す.
; (make-machine ⟨register-names⟩ ⟨operations⟩ ⟨controller⟩)
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

; レジスタ
; 手続きmake-registerはアクセスしたり, 変更したり出来る値を保持するレジスタを作り出す:
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else (error "unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

; スタック
; 手続きmake-stackはその局所状態が, スタックの項目のリストからなるスタックを作り出す. スタックは項目をスタックにpushし, 最上の項目をスタックから外して返してpopし, またスタックを空にinitializeする要求を受け入れる:
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)

    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

; 基本計算機
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply define register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))

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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

; 与えられた計算機の実行をシミュレートする. 制御列の先頭から実行開始し, その列の最後に達した時に停止する.
; (start ⟨machine-model⟩)
(define (start machine)
  (machine 'start))

; 与えられた計算機のシミュレートされるレジスタの内容を返す.
; (get-register-contents ⟨machine-model⟩ ⟨register-name⟩)
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

; 与えられた計算機のシミュレートされるレジスタに値を格納する.
; (set-register-contents! ⟨machine-model⟩ ⟨register-name⟩ ⟨value⟩)
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

; 5.2.2 アセンブラ

; assemble手続きはアセンブラへの主要な入り口である. 引数として制御器の文書と計算機のモデルをとり, モデルに格納すべき命令列を返す.
; assembleはextract-labelsを呼び出し, 渡された制御器文書から, 最初の命令リストとラベル表を構築する.
; extract-labelsの第二引数は, これらの結果を処理するのに呼び出す手続きである: この手続きはupdate-insts!を使い, 命令実行手続きを生成し, それらを命令リストに挿入して, 修正したリストを返す.
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

; 問題 5.8
; 3.3.3から、assocを持ってきた
; assocはcarに与えられたキーを持つレコードを返す.
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

; extract-labelsは引数としてリストtext(制御器の命令の式の列)と, receive手続きをとる. receiveは二つの値: (1)それぞれがtextの命令を含んでいる命令のデータ構造のリストinstsと(2)textの各ラベルを, リストinsts内のラベルが指示している位置と対応づけるlabelsという表で呼び出される.
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (assoc next-inst labels) ; 5.8
                                  (error "exist same label:" next-inst)
                              (receive insts
                                       (cons (make-label-entry next-inst insts)
                                             labels)))
                              (receive (cons (make-instruction next-inst) insts)
                                       labels)))))))
; update-insts!は, 最初命令の文書を持っていただけの命令リストを, 対応する実行手続きを含むように修正する:
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
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

; 5.2.3 命令の実行手続きの生成
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
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

; assign命令
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
      (lambda () ; assign の実行手続き
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

; goto命令
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
              

; その他の命令
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

; 部分式の実行手続き

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
                (cond
                  ((constant-exp? e) ; 5.9
                   (make-primitive-exp e machine labels))
                  ((register-exp? e) ; 5.9
                   (make-primitive-exp e machine labels))
                  (else
                   (error "Invalid operand for operation -- ASSEMBLE" e))))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op
             (map (lambda (p) (p))
                  aprocs)))))

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

; quoted?は, 指示した記号で始るリストを識別する手続きtagged-list?を使って定義する:
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


; 5.2.4 計算機の性能の監視

#|
; 基本計算機モデルにスタック統計量を印字する演算を追加する.
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))
|#

(define stack-test-machine
  (make-machine
   '(x y)
   '()
   '(
     (assign x (const 10))
     (assign y (const 20))

     (save x)
     (save y)

     (restore x)
     (restore y)
     )))

(start stack-test-machine)

(get-register-contents stack-test-machine 'x)
(get-register-contents stack-test-machine 'y)

#|
     (restore x)
     (restore y)
だと
10
20

     (restore y)
     (restore x)
だと
20
10
|#


(define fib-machine
  (make-machine
   '(n val continue)
   (list
    (list '< <)
    (list '- -)
    (list '+ +))
   '(
     (assign continue (label fib-done))

     fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))

       (save continue)
       (assign continue (label afterfib-n-1))
       (save n)
       (assign n (op -) (reg n) (const 1))
       (goto (label fib-loop))

     afterfib-n-1
       (restore n)
       (restore continue)

       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val)
       (goto (label fib-loop))

     afterfib-n-2

       ;; 5.11 a
       ;; afterfib-n-2 に来た時点
       ;; val = Fib(n-2)
       ;; stack の一番上 = Fib(n-1)
     
       ;; 元のコード

       ;; (assign n (reg val))
       ;; n = Fib(n-2)
       ;; val = Fib(n-2)

       ;; (restore val)
       ;; n = Fib(n-2)
       ;; val = Fib(n-1)

       ;; 今回
       (restore n)
       ;; val = Fib(n-2)
       ;; n = Fib(n-1)

       (restore continue)
       (assign val
               (op +) (reg val) (reg n))
       (goto (reg continue))

     immediate-answer
       (assign val (reg n))
       (goto (reg continue))

     fib-done
     )))

(set-register-contents! fib-machine 'n 5)

(start fib-machine)

(get-register-contents fib-machine 'val)