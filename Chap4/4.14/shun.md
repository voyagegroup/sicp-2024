evaはinputに以下のようにmapの定義を入れた。

```scheme
;;; M-Eval input:
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))
```

louisは基本手続きに追加した

```scheme
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        ;; ... 他の基本手続き ...
        
        (list 'map map) ;追加
        ))
```

mapを使った以下の式を評価する場合を考える。

```scheme
(map (lambda (n) (+ n 1)) '(1 2 3))
```

基本手続として登録した場合以下のようなエラーになった。

```
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: (procedure (n) ((+ n 1)) (((false true car cdr cons null? eq? + - * / = cadr assoc list map) #f #t (primitive #<procedure:mcar>) (primitive #<procedure:mcdr>) (primitive #<procedure:mcons>) (primitive #<procedure:null?>) (primitive #<procedure:eq?...
```
このgivenとして出てきているのはmake-procedureによって作られたリストであり、リストを関数として実行しているというエラーである。

```scheme
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
```

このようになるのは超循環評価器においてmapに渡されるのは、inputの式そのものではなく我々が評価した結果だからだ。

`(map (lambda (n) (+ n 1)) '(1 2 3))`をevalに渡すと、実行部にあたる。

```scheme
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
  ...
  ((application? exp)
           (my-apply (eval (operator exp) env)
                     (list-of-values (operands exp) env)))
```

applyの定義は以下のようになっている。そこで基本手続を作用させることになる。

```scheme
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)) ;基本演算を作用させる
        ...
```

ただし、applyに渡す第二引数には引数のリストの内容をevalしていったものが入る。

```scheme
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
```

引数がevalされると、先ほど見たmake-procedureが実行される。

```scheme
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
  ...

  ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
```

基本手続を作用させる部分は以下のようになっている。

```scheme
(define (primitive-implementation proc) (cadr proc))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
```

このとき`(apply-in-underlying-scheme (primitive-implementation proc) args)`の実行は、以下のようになってしまい、リストを実行するという挙動になるためうまくいかない。

```scheme
(apply map (procedure (n) ((+ n 1)) (環境のリスト)) (1 2 3))
```

反対にmapを独自で定義した場合は、map自身も評価器を通した上で実行していくため成功する。
