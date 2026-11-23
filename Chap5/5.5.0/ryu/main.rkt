#lang sicp

(#%require racket/include)


(include "compiler.rkt")
(include "syntax.rkt")
(include "main.rkt")


(statements
 (compile '(+ 1 2) 'val 'return))

#|
Welcome to DrRacket, version 8.12 [cs].
Language: sicp, with debugging; memory limit: 128 MB.

((assign proc (op lookup-variable-value) (const +) (reg env))
 (assign val (const 2))
 (assign argl (op list) (reg val))
 (assign val (const 1))
 (assign argl (op cons) (reg val) (reg argl))
 (test (op primitive-procedure?) (reg proc))
 (branch (label primitive-branch1))
 compiled-branch2
 (assign continue (label after-call3))
 (assign val (op compiled-procedure-entry) (reg proc))
 (goto (reg val))
 primitive-branch1
 (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call3)
> 
|#

