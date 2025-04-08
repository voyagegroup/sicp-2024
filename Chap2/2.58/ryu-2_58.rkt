#lang sicp
; 微分プログラムを修正し, 前置演算子でなく+や*が中置きになっている通常の数学の式に動作させたいと思う. 微分のプログラムは抽象データを使って定義してあるので, 微分プログラムが操作する代数式の表現を定義する述語, 選択子, 構成子だけを変更するだけで, 式の別の表現形で動作するように出来る.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))


; • 変数は記号とする. 基本手続き symbol?で識別出来る:
(define (variable? x) (symbol? x))

; • 二つの変数はそれを表現している記号がeq?なら同じである:
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


; • 和と積はリストとして構成する:
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))


; • 和は2つ目の要素が記号+であるリストである:
(define (sum? x)
;   (and (pair? x) (eq? (car x) '+)))
  (and (pair? x) (eq? (cadr x) '+)))

; • 加数は和のリストの第1項である:
(define (addend s) (car s))

; • 被加数は和のリストの第三項である:
(define (augend s) (caddr s))

; • 積は2つ目の要素が記号*であるリストである:
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

;• 乗数は積のリストの第1項である:
(define (multiplier p) (car p))

; • 被乗数は積のリストの第三項である:
(define (multiplicand p) (caddr p))


; --- ここから新規 ---

(deriv '(x + 1) 'x) 
(deriv '(x + (3 * (x + (y + 2)))) 'x)
; x + 3(x + y + 2)
; 4x + y + 2
; -> 4