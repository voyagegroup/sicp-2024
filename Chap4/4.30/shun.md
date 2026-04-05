## a

```scheme
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

;;; L-Eval input:
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
57
321
88
;;; L-Eval value:
done
```

Cyは遅延評価においてはその値が必要な時にのみ評価されると理解しており、副作用のようなその返り値を使用しない処理についてはスキップされると懸念している。
しかし、displayについては合成手続きではなく基本手続きであるため、サンクになることなく実行される。

```scheme
(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ; 変更した
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; 変更した
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
```

## b

```scheme
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
```

### 元々の版

```scheme
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
```

```scheme
;;; L-Eval input:
(p1 1)

;;; L-Eval value:
(1 2)

;;; L-Eval input:
(p2 1)

;;; L-Eval value:
1
```

### Cy版

```scheme
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
```

```scheme
;;; L-Eval input:
(p1 1)

;;; L-Eval value:
(1 2)

;;; L-Eval input:
(p2 1)

;;; L-Eval value:
(1 2)
```

### p2の結果の違いについて

元々の版ではeval-sequenceはサンクをforceする機能を持たない。
evalにサンクを渡してもサンクを返すだけになる。
つまり、pを実行するとき、引数であるset手続きのサンクはeを評価してもサンクを返すだけで内部の処理は実行されない。結果としてxは1のままであり、それがp2の結果として返される。
他方Cyの版はforceするため、(1 2)がxにsetされて(1 2)が結果として返る。

## c

```scheme
(define (actual-value exp env)
  (force-it (eval exp env)))
```

Cyの版でも、actual-valueを呼び出した時にevalされ、force-itはサンクでなければそれをただ返すだけなので振る舞いに影響はない。


## d

引数に与えた手続きを呼び出したのならそれは実行されると私は考える。
よってCyの挙動を支持する。
手続きが実行されない本文の解決法は好ましくない。
ただし、aの場合ではforce-itを経由する分不必要なオーバーヘッドが生じるため解決法自体はあまり好ましくない。