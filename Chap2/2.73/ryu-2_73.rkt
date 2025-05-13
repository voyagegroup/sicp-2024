#lang sicp
; get/put（3.3.3より）
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

; 2.3.2
; • 変数は記号とする. 基本手続き symbol?で識別出来る:

(define (variable? x) (symbol? x))
; • 二つの変数はそれを表現している記号がeq?なら同じである:

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
; • 和と積はリストとして構成する:

(define (make-sum a1 a2) (list '+ a1 a2))


(define (make-product m1 m2) (list '* m1 m2))
; • 和は最初の要素が記号+であるリストである:

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
; • 加数は和のリストの第二項である:

(define (addend s) (cadr s))
; • 被加数は和のリストの第三項である:

(define (augend s) (caddr s))
; • 積は最初の要素が記号*であるリストである:

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
; • 乗数は積のリストの第二項である:

(define (multiplier p) (cadr p))
; • 被乗数は積のリストの第三項である:

(define (multiplicand p) (caddr p))

 (define (deriv-old exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-old (addend exp) var)
                   (deriv-old (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv-old (multiplicand exp) var))
           (make-product (deriv-old (multiplier exp) var)
                         (multiplicand exp))))
        ; ⟨更に多くの規則をここに追加出来る.⟩
        (else (error "unknown expression type -- DERIV" exp))))

(deriv-old  '(+ x 3) 'x)
; (+ 1 0)

; コピペ
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; 2.73 ここから
; a. 上でやったことを説明せよ. 述語number?やvariable?がデータ主導の振分けに吸収出来ないのはなぜか.

; (else ((get 'deriv (operator exp)) (operands exp) var))))
; この時点で、operatorとoperandsに分けて、getの呼び出しをしている。
; もし、組み込もうとしても、operator、operandsでエラーになってしまう。
; (operator 3) ; エラー
; (operands 3) ; エラー

; b. 和と積の微分の手続きを書き, 上のプログラムで使う表に, それらを設定するのに必要な補助プログラムを書け.

; sum
(define (install-sum-package)
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))

  (put 'deriv '+ deriv-sum)
  'done)

; 実行
(install-sum-package)
(deriv '(+ x 3) 'x)
; (+ 1 0)

; product
(define (install-product-package)
  (define (deriv-product operands var)
    (display (car operands))
    (display (cadr operands))
    (make-sum (make-product (car operands) (deriv (cadr operands) var))
              (make-product (deriv (car operands) var) (cadr operands))))

  (put 'deriv '* deriv-product)
  'done)

(install-product-package)
(deriv '(* x y) 'x)
; yx -> displayのログ
; (+ (* x 0) (* 1 y))

; c. (問題2.56)のべき乗のような, その他の微分規則を選び, このデータ主導システムに設定せよ.
(define (install-exp-package)
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
  
  (define (deriv-exp operands var)
    (let ((base (car operands))
          (exponent (cadr operands)))
      (cond ((and (number? exponent)
                  (variable? base)
                  (same-variable? base var))
             (make-product
              exponent
              (make-product
               (make-exponentiation base (- exponent 1))
               (deriv base var))))
            (else
             (error "Exponent rule not implemented for this form"
                    (list base exponent))))))

  (put 'deriv '** deriv-exp)
  'done)

(install-exp-package)

(deriv '(** x 3) 'x)
; (* 3 (* (** x 2) 1))

; d. この代数式操作では, 式の型はそれを結合している代数演算である. しかし手続きの目印を反対にし, derivの振分けを
; ((get (operator exp) 'deriv) (operands exp) var)
; のようにしたとしよう. 微分システムには対応したどのような変更が必要か.

; putの引数を逆にすれば良い




