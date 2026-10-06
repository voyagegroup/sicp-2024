#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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

(define (make-register name)
  ; 5.18 解答
  (let ((contents '*unassigned*)
        (trace-enabled false))
    ; ここまで
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             ; 5.18 解答
             (lambda (value)
               (if trace-enabled
                   (begin
                     (display (list name contents value))
                     (newline))
                   'done)
               (set! contents value)))
            ((eq? message 'trace-on)
             ; 5.18 解答
             (set! trace-enabled true))
            ((eq? message 'trace-off)
             (set! trace-enabled false))
             ; ここまで
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

; スタック

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

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

; 基本計算機

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0)
        (trace-enabled false) ; 5.16 解答
        ; 5.19 解答
        (labels '())
        (breakpoints '())
        (stopped-at-breakpoint false))
        ; ここまで
    (let ((the-ops
           (list (list 'initialize-stack
            (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
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
              (error "Unknown register:" name))))
      ; 5.19 解答
      (define (nth-instruction insts offset)
        (cond ((< offset 0)
               (error "Breakpoint offset must be non-negative" offset))
              ((null? insts)
               (error "Breakpoint offset exceeds instruction sequence" offset))
              ((= offset 0) (car insts))
              (else (nth-instruction (cdr insts) (- offset 1)))))
      (define (breakpoint-instruction label offset)
        (nth-instruction (lookup-label labels label) offset))
      (define (breakpoint? inst)
        (memq inst breakpoints))
      (define (remove-breakpoint inst breakpoints)
        (cond ((null? breakpoints) '())
              ((eq? inst (car breakpoints))
               (remove-breakpoint inst (cdr breakpoints)))
              (else
               (cons (car breakpoints)
                     (remove-breakpoint inst (cdr breakpoints))))))
      (define (execute-instruction inst)
        (set! instruction-count (+ instruction-count 1)) ; 5.15 解答
        ; 5.17 解答
        (cond (trace-enabled
               (for-each
                (lambda (label)
                  (display label)
                  (newline))
                (instruction-labels inst))
               (display (instruction-text inst))
               (newline)))
        ; ここまで
        ((instruction-execution-proc inst)))
      (define (proceed)
        (if stopped-at-breakpoint
            (begin
              (set! stopped-at-breakpoint false)
              (execute-instruction (car (get-contents pc)))
              (execute))
            (error "Machine is not stopped at a breakpoint -- PROCEED")))
      ; ここまで
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              ; 5.19 解答
              (if (breakpoint? (car insts))
                  (begin
                    (set! stopped-at-breakpoint true)
                    'breakpoint)
                  (begin
                    (execute-instruction (car insts))
                    (execute))))))
              ; ここまで
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (set! stopped-at-breakpoint false); 5.19 解答
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ; 5.19 解答
              ((eq? message 'install-labels)
               (lambda (new-labels) (set! labels new-labels)))
              ((eq? message 'proceed) (proceed))
              ((eq? message 'set-breakpoint)
               (lambda (label offset)
                 (let ((inst (breakpoint-instruction label offset)))
                   (if (not (breakpoint? inst))
                       (set! breakpoints (cons inst breakpoints))
                       'done))))
              ((eq? message 'cancel-breakpoint)
               (lambda (label offset)
                 (set! breakpoints
                       (remove-breakpoint
                        (breakpoint-instruction label offset)
                        breakpoints))))
              ((eq? message 'cancel-all-breakpoints)
               (set! breakpoints '()))
              ; ここまで
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ; 5.15 解答
              ((eq? message 'print-instruction-count)
               (display instruction-count))
              ((eq? message 'reset-instruction-count)
               (set! instruction-count 0))
              ; ここまで
              ; 5.16 解答
              ((eq? message 'trace-on)
               (set! trace-enabled true))
              ((eq? message 'trace-off)
               (set! trace-enabled false))
              ; ここまで
              ; 5.18 解答
              ((eq? message 'trace-register)
               (lambda (register-name)
                 ((lookup-register register-name) 'trace-on)))
              ((eq? message 'untrace-register)
               (lambda (register-name)
                 ((lookup-register register-name) 'trace-off)))
              ; ここまで
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
                    ((machine 'install-labels) labels) ; 5.19 解答
                    insts)))

; 5.8解答: ラベルの追加直前にすでにラベルがないかを調査する
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (assoc next-inst labels)
                                  (error "Duplicate label -- ASSEMBLE"
                                         next-inst)
                                  ; 5.17 解答
                                  (begin
                                    (if (not (null? insts))
                                        (add-instruction-label! (car insts)
                                                                next-inst)
                                        'done)
                                    (receive insts
                                             (cons (make-label-entry next-inst
                                                                     insts)
                                                   labels))))
                                  ; ここまで
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
         (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

; 5.17 解答
(define (make-instruction text)
  (list text '() '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-labels inst)
  (cadr inst))

(define (instruction-execution-proc inst)
  (caddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cddr inst) proc))

(define (add-instruction-label! inst label)
  (set-car! (cdr inst)
            (cons label (instruction-labels inst))))
; ここまで

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

; 型に従って振分け
(define (make-execution-procedure inst labels machine pc flag stack ops)
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

; assign 命令
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

; goto 命令
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
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
                  (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine (register-exp-reg dest))))
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

; 5.9解答
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (or (constant-exp? e) ; ここで const と req だけ許可する
                        (register-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "Operation operand must be a constant or register -- ASSEMBLE"
                           e)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list?  (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

; 5.21 解答
; a. 再帰的な count-leaves
(define count-leaves-recursive-machine
  (make-machine
   '(tree val continue temp)
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +))
   '(controller
       (assign continue (label done))

     count-leaves
       (test (op null?) (reg tree))
       (branch (label empty-tree))
       (test (op pair?) (reg tree))
       (branch (label pair-tree))
       (assign val (const 1))
       (goto (reg continue))

     empty-tree
       (assign val (const 0))
       (goto (reg continue))

     pair-tree
       (save continue)
       (save tree)
       (assign tree (op car) (reg tree))
       (assign continue (label after-car))
       (goto (label count-leaves))

     after-car
       (restore tree)
       (save val)
       (assign tree (op cdr) (reg tree))
       (assign continue (label after-cdr))
       (goto (label count-leaves))

     after-cdr
       (restore temp)
       (restore continue)
       (assign val (op +) (reg temp) (reg val))
       (goto (reg continue))

     done)))

; b. 明示的なカウンタを持つ count-leaves
(define count-leaves-iterative-machine
  (make-machine
   '(tree n val continue)
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +))
   '(controller
       (assign n (const 0))
       (assign continue (label done))
       (goto (label count-iter))

     count-iter
       (test (op null?) (reg tree))
       (branch (label empty-tree))
       (test (op pair?) (reg tree))
       (branch (label pair-tree))
       (assign n (op +) (reg n) (const 1))
       (goto (reg continue))

     empty-tree
       (goto (reg continue))

     pair-tree
       (save continue)
       (save tree)
       (assign tree (op car) (reg tree))
       (assign continue (label after-car))
       (goto (label count-iter))

     after-car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (goto (label count-iter))

     done
       (assign val (reg n)))))

(define (run-count-leaves-test name machine tree)
  (set-register-contents! machine 'tree tree)
  (start machine))

(define test-tree '((1 2) (3 (4 . 5)) () 6))

(run-count-leaves-test 'recursive
                       count-leaves-recursive-machine test-tree)
(run-count-leaves-test 'iterative
                       count-leaves-iterative-machine test-tree)
; ここまで
