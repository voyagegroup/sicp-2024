#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (get op type)
  (op))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; derivの使い方
; (deriv '(+ x 3) 'x)
; 1

; a
; deriv演算と型で手続きを取得し、それをオペランド((+ 3 5なら3,5))とvarに対して適用する
; number?やvariable?は単一の値に対する操作でタグを必要としないため、データ主導に組み込めない

; b
;和
(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s)
    (if (null? (cddr s))
        (cadr s)
        (cons '+ (cdr s))))
  (define (make-sum a1 a2)
    (cond ((number? a1 0) a2)
          ((number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'make '+ make-sum)
  (put 'deriv '+ derive-sum)
  'done)

;積
(define (install-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (if (null? (cddr p))
        (cadr p)
        (cons '* (cdr p))))
  (define (make-product p1 p2)
    (cond ((or (= p1 0) (= p2 0)) 0)
          ((and (number? p1) (number p2)) (* p1 p2))
          (else (list '* p1 p2))))
  (define (deriv-product exp var)
    (make-product (deriv (multiplier exp) var)
                  (deriv (multiplicand exp) var)))
  (put 'make '* make-product)
  (put 'deriv '* deriv-product)
  'done)

; c
;べき乗
(install-product-package)
(define (install-exponentiation-package)
  (define (base p) (car p))
  (define (exponent p) (cadr p))
  (define (make-exponentiation b e)
    (cond ((= e 0) 0)
          ((= e 1) b)
          (else (list '** b e))))
  (define (deriv-exponentiation exp var)
    (let (make-p (get 'make '*))
      (make-p
       (exponent exp)
       (make-exponentiation (base exp) (- (exponent exp) 1)))))
  (put 'make '** make-exponentiation)
  (put 'deriv '** deriv-exponentiation))

; d
;  (get ⟨op⟩  ⟨type⟩)を (get ⟨type⟩ ⟨op⟩)にし、putの順番も(put ⟨type⟩ ⟨op⟩ ⟨item⟩)で入れ替える