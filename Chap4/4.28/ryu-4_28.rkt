#lang sicp

; もともと
(define (eval exp env)
  (cond
    ...
    ((application? exp)
     (my-apply (eval (operator exp) env)
               (list-of-values (operands exp) env)))))

; 4.2.2 遅延評価
(define (eval exp env)
  (cond
    ...
    ((application? exp)
     (my-apply (actual-value (operator exp) env)
            (operands exp)
            env))))

#|
- (my-apply (actual-value (operator exp) env)
+ (my-apply (eval (operator exp) env)

に変更して以下を実行

((lambda (f) (f 1 2))
 (if true + -))

;;; M-Eval input:
((lambda (f) (f 1 2))
 (if true + -))
exp((lambda (f) (f 1 2)) (if true + -))
((f 1 2))
. . Unknown procedure type -- APPLY (thunk (if true + -) (((false . #f) (true . #t) (car primitive #<procedure:mcar>) (cdr primitive #<procedure:mcdr>) (cons primitive #<procedure:mcons>) (null? primitive #<procedure:null?>) (+ primitive #<procedure:+>) (- primitive #<procedure:->) ...
> 


apply に渡された operator が thunk のまま
この時
operator: (lambda (f) ...)
operand: (if true + -)

operandは遅延される

(thunk (if true + -) env)
これがmy-applyに渡って

(primitive-procedure? thunk) → false
(compound-procedure? thunk) → false

で落ちた。

--
演算子の値も thunk になる場合があるため、eval では thunk のまま apply に渡されてしまう。
actual-value を使うことで thunk を強制し、手続き(この場合は +)を得てから apply に渡す必要がある。

|#
