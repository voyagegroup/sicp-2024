#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (and (number? m1) (= m1 0)) (and (number? m2) (= m2 0))) 0)
        ((and (number? m1) (= m1 1)) m2)
        ((and (number? m2) (= m2 1)) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

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

(define (parse exp)
  (cond
    ((symbol? exp) exp)
    ((number? exp) exp)
    ((eq? '+ (car exp)) exp)
    ((eq? '* (car exp)) exp)
    ((= (length exp) 3) (list (cadr exp) (parse (car exp)) (parse (caddr exp))))
    ((> (length exp) 4)
     (cond ((eq? (cadr exp) (cadddr exp))
            (parse (cons (list (cadr exp) (parse (car exp)) (parse (caddr exp))) (cdddr exp))))
           ((and (eq? (cadr exp) '+) (eq? (cadddr exp) '*))
            (parse (cons (car exp) (cons (cadr exp) (cons (list (cadddr exp) (parse (caddr exp)) (parse (list-ref exp 4))) (list-tail exp 5))))))))))

(parse '(x + 3 * (x + y + 2)))
; '(+ x (* 3 (+ (+ x y) 2)))

(deriv (parse '(x + 3 * (x + y + 2))) 'x)
; 4
