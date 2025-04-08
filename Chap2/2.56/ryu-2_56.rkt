#lang sicp

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
        ((exponentiation? exp)
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1))))
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
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))


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

;• 乗数は積のリストの第二項である:
(define (multiplier p) (cadr p))

; • 被乗数は積のリストの第三項である:
(define (multiplicand p) (caddr p))


; --- ここから新規 ---
; 適切な手続きexponentiation?, base, exponentおよびmake-exponentiationを定義して実装せよ. (べき乗を表すのに記号**を使おう.) 何かの0乗は1で, 何かの1乗はそれ自身という規則を組み込め.

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 0)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(deriv '(** x 3) 'x)
; x^3
; -> (* 3 (** x 2))

(deriv '(** (+ x 1) 3) 'x)

(deriv '(** (* x x) 3) 'x)





