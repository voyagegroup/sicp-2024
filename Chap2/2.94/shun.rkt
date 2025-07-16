#lang sicp

(define coercion-list '())

(define (clear-coercion-list)
  (set! coercion-list '()))

(define (put-coercion type1 type2 item)
  (if (get-coercion type1 type2) coercion-list 
      (set! coercion-list
            (cons (list type1 type2 item)
                  coercion-list))))

(define (get-coercion type1 type2)
  (define (get-type1 listItem)
    (car listItem))
  (define (get-type2 listItem)
    (cadr listItem))
  (define (get-item listItem)
    (caddr listItem))
  (define (get-coercion-iter list type1 type2)
    (if (null? list) #f
        (let ((top (car list)))
          (if (and (equal? type1 (get-type1 top))
                   (equal? type2 (get-type2 top))) (get-item top)
                   (get-coercion-iter (cdr list) type1 type2)))))
  (get-coercion-iter coercion-list type1 type2))

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

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((number? datum) 'scheme-number)
    ((pair? datum) (car datum))
    (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (square n)
  (* n n))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y)
  (apply-generic 'equ? x y))
(define (=zero? z)
  (apply-generic '=zero? z))
(define (raise x)
  (apply-generic 'raise x))
(define (project z)
  (apply-generic 'project z))

(define (install-rectangular-package)
   ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  

   ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
   ;; 内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

   ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)



(define (install-complex-package)
   ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

   ;; 内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (= (real-part z) 0) (= (imag-part z) 0)))
  (define (project z)
    (make-real (real-part z)))

   ;; システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'project '(complex) project)
  'done)

; 複素数
(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
 
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; 実数
(define (install-real-package)
  (define (tag z) (attach-tag 'real z))
  (define (raise x)
    (make-complex-from-real-imag x 0))
  (define (project x)
    (make-rational (numerator x) (denominator x)))

  (put 'raise '(real) raise)
  (put 'project '(real) project)
  )

(install-real-package)

(define (make-real x) (attach-tag 'real x))

; 有理数
(define (install-rational-package)
   ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
  (define (add-rat x y)
    (make-rat (apply-generic 'add (apply-generic 'mul (numer x) (denom y))
                 (apply-generic 'mul (numer y) (denom x)))
              (apply-generic 'mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  (define (=zero? z)
    (= (numer z) 0))
  (define (raise x)
    (make-real (/ (car x) (cdr x))))
  (define (project x)
    (numer x))

   ;; システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational) raise)
  (put 'project '(rational) project)
  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))
  (put '=zero? '(scheme-number)
       (lambda (n) (= n 0)))
  (put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  'done)

(install-scheme-number-package)

(define (make-integer x) (attach-tag 'scheme-number x))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)


(define (find-index lst element)
  (define (helper lst element index)
    (cond
      [(null? lst) #f]
      [(equal? (car lst) element) index]
      [else (helper (cdr lst) element (+ index 1))]))
  (helper lst element 0))

(define tower '(scheme-number rational real complex polynomial))

(define (down x)
  (if (or (= (find-index tower (type-tag x)) 0) (not (equal? x (raise (project x)))))
      x
      (down (project x))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (if (<= (length args) 2)
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (= (length args) 2)
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                  (let ((f1 (find-index tower type1))
                        (f2 (find-index tower type2))
                        )
                    (cond
                          ((eq? type1 type2)
                           (down (apply-generic op a1 a2)))
                          ((< f1 f2)
                           (down (apply-generic op (apply-generic 'raise a1) a2)))
                          ((> f1 f2)
                           (down (apply-generic op a1 (apply-generic 'raise a2))))
                          (else
                           (error "No method for these types"
                                  (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags)))))
        (let* ((first (car args))
                         (second (cadr args))
                         (rest (cddr args))
                         (result (apply-generic op first second)))
                    (apply apply-generic op (cons result rest)))
        )
    ))



(define (install-polynomial-package)
   ;; 内部手続き
   ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p)  (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ; 2.87
  (define (zero?-poly p)
    (zero?-terms (term-list p)))
  (define (zero?-terms L)
    (or (empty-termlist? L)
        (and (=zero? (first-term L)) (zero?-terms (rest-terms L)))))

  (define (sign-inversion p)
    (define (sign-inversion-coeff c)
      (- c))
    (define (sign-inversion-term-list tl)
      (map))
    (make-poly (variable p) (sign-inversion-term-list (term-list p))))

   ;; 項と項リストの表現
  (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (apply-generic 'add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (negative p)
    (make-poly 
        (variable p) 
        (map list
            (map order (term-list p))
            (map (lambda (x) (* -1 (coeff x))) (term-list p))))
  )
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (apply-generic 'mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

  ; 2.91
  (define (sub-terms L1 L2)
    (add-terms L1 (negative-tl L2)))

  (define (negative-tl tl)
  (map (lambda (term) (make-term (order term) (* -1 (coeff term)))) tl))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car result)) (make-poly (variable p1) (cadr result))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  (define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (apply-generic 'div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms (sub-terms L1 (mul-terms (list (list new-o new-c)) L2)) L2)
                     ))
                (list (adjoin-term (list new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                ))))))


  ; 2.94

(define (greatest-common-divisor n1 n2)
  (define (poly? n)
    (and (pair? n) (symbol? (car n)) (list? (cdr n))))
  (if (or (poly? n1) (poly? n2)) 
      (gcd-poly n1 n2)
      (gcd n1 n2)))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                 (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- gcd-POLY"
               (list p1 p2))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))
  
   ;; システムの他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       =zero?)
  (put 'sub '(polynomial polynomial) 
    (lambda (p1 p2) (tag (add-poly p1 (negative p2)))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (greatest-common-divisor p1 p2))))
  'done)



(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(apply-generic 'greatest-common-divisor p1 p2)
; (polynomial x (2 -1) (1 1))


; p1 = x (x^3 - x^2 - 2x + 2)
;    = x (x^2 - 2) (x - 1)
; p2 = x (x^2 - 1)
;   = x (x - 1) (x + 1)

; x (x - 1) = (x^2 - x) が最大公約数

; 手計算と機械計算が異なるが、問題を素直に解くとこうなりそうでよくわからん

