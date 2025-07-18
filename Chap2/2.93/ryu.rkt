#lang sicp


; -- 2.86

(define (square-root x) (apply-generic 'square-root x))
(define (arctangent y x) (apply-generic 'arctangent y x))


; --- 2.4.2
(define (attach-tag type-tag contents)
  (cons type-tag contents))


(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
; ---

; --- 2.4.3
; apply-genericは演算の名前と引数の型から表を探し, 得られた手続きがあればそれを作用させる
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (error
;             "No method for these types -- APPLY-GENERIC"
;             (list op type-tags))))))
; ---

; --- 3.3.3 get/put
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
; ---

; --- apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ; 2.85
          ; (apply proc (map contents args)) もともと
          ; (drop (apply proc (map contents args))) ループした?
          (let ((result (apply proc (map contents args))))
            (if (or (eq? op 'raise) ; drop自体がapply-genericを呼ぶ演算は除外する必要があった
                    (eq? op 'project)
                    (eq? op 'equ?)
                    (eq? op 'level)
                    (eq? op '=zero?)
                    (eq? op 'add)
                    (eq? op 'sub)
                    (eq? op 'mul)
                    (eq? op 'div)
                    (eq? op 'negate))
                result
                (drop result)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "同じ型で変換しようとしています")
                    ; このあたりを変更
                    (cond ((higher-type? type1 type2)
                           ; type1の方が高い → a2をtype1まで raise
                           (apply-generic op a1 (raise-to type1 a2)))
                          ((higher-type? type2 type1)
                           ; type2の方が高い → a1をtype2まで raise
                           (apply-generic op (raise-to type2 a1) a2))
                          (else
                           (error "No method for these types"
                                  (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


; https://gist.github.com/kinoshita-lab/b76a55759a0d0968cd97
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
; ---

; --- ここから ----
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x)) ; これを定義した

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

  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))

  ;; int → rational
  (put 'raise '(scheme-number)
;        (lambda (x) (make-rational x 1)))
       (lambda (x) (make-rational (make-scheme-number x) (make-scheme-number 1))))

  (put 'level 'scheme-number 1) ;levelを追加

  ;; 2.85
  (put 'project '(scheme-number)
       (lambda (x) (tag x)))  ; 最下層なので自分自信をそのまま帰す
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))

  ;; 2.86
  (put 'square-root '(scheme-number)
       (lambda (x) (tag (sqrt x))))
  (put 'arctangent '(scheme-number scheme-number)
       (lambda (y x) (tag (atan y x))))

  ;; 2.88
  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;(define (make-rat n d)
  ;  (let ((g (gcd n d)))
  ;    (cons (/ n g) (/ d g))))
  ; 2.93: gcdによる約分を行わないようにした
  (define (make-rat n d)
    (cons n d))

  ; 2.93: + などを汎用算術パッケージに
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

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

  (put '=zero? '(rational)
       ; (lambda (x) (= (numer x) 0))) 2.93 =zero?を使うように
       (lambda (x) (=zero? (numer x))))

  ;; rational → real
  (put 'raise '(rational)
       ;(lambda (x) (make-real (/ (numer x) (denom x)))))
       (lambda (x) 

         (let ((result (div (numer x) (denom x))))
           (if (eq? (type-tag result) 'real)
               result  ; 既に real なら
               (make-real (contents result))))))  ; contents を取り出して real に
  (put 'level 'rational 2); 追加
 

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  ;; 2.85
  (put 'project '(rational)
       (lambda (x)
         (if (equ? (denom x) (make-scheme-number 1))  ; 分母が1の場合のみ ; 2.93 1をタグ付きへ
             (numer x)
             (tag x))))  ; 変換できない場合はrational のまま
  (put 'equ? '(rational rational)
       (lambda (x y) 
         (and (equ? (numer x) (numer y)) ; 2.93 = → equ?へ
              (equ? (denom x) (denom y)))))

  ;; 2.86
  (put 'square-root '(rational)
       (lambda (x) 
         ;; contents を取り出してから生の演算
         (let ((num-val (contents (numer x)))
               (den-val (contents (denom x))))
           (make-real (sqrt (/ num-val den-val))))))

  (put 'arctangent '(rational rational)
       (lambda (y x) 
         ;; contents を取り出してから生の演算
         (let ((y-num (contents (numer y)))
               (y-den (contents (denom y)))
               (x-num (contents (numer x)))
               (x-den (contents (denom x))))
           (make-real (atan (/ y-num y-den) (/ x-num x-den))))))

  ;; 2.88
  (put 'negate '(rational)
       (lambda (x) (tag (make-rat (negate (numer x)) (denom x)))))
  'done)


(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  ;; システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'real x))
  
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'level 'real 3); 追加


  (put 'make 'real
       (lambda (x) (tag x)))

  (put '=zero? '(real)
       (lambda (x) (= x 0.0)))

  ;; 2.85で追加
  (put 'project '(real)
       (lambda (x)
         (if (integer? x) ; 5.0 -> #t, 5.1 -> #f
             (make-rational (make-scheme-number (round x)) (make-scheme-number 1)) ; 変換できる場合はroundする
          
             (tag x))))   ; 変換できない場合はreal のまま
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))

  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag (tag x) (make-scheme-number 0))))

  ;; 2.86
  (put 'square-root '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'arctangent '(real real)
       (lambda (y x) (tag (atan y x))))
  ;; 2.88
  (put 'negate '(real)
       (lambda (x) (tag (- x))))
  'done)

(define (make-real x)
  ((get 'make 'real) x))

;; 複素数パッケージ
(define (install-complex-package)
  ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (angle z)
    (arctangent (imag-part z) (real-part z)))


  ;; 内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2)) ; 2.86 + -> add
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2)) ; 2.86 - -> sub
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2)) ; 2.86 mul, addへ
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2)) ; 2.86 div, subへ
                       (sub (angle z1) (angle z2))))

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

  (put '=zero? '(complex)
       (lambda (z) 
         (and (=zero? (real-part z)) 
              (=zero? (imag-part z)))))

  ; 追加
  (put 'real-part '(complex)
       (lambda (z) (apply-generic 'real-part (contents z))))
  (put 'imag-part '(complex)
       (lambda (z) (apply-generic 'imag-part (contents z))))

  (put 'magnitude '(complex)
       (lambda (z) (apply-generic 'magnitude (contents z))))
  (put 'angle '(complex)
       (lambda (z) (apply-generic 'angle (contents z))))
  
  (put 'level 'complex 4)
  ; 最上位の型なのでprojectしない
  (put 'project '(complex)
       (lambda (z) (tag z)))


  ;; 2.88
  (put 'negate '(complex)
       (lambda (z) 
         (make-complex-from-real-imag 
          (negate (real-part z))
          (negate (imag-part z)))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rectangular-package) ; 2.4.3からコピペ
  (define (square x) (mul x x)) ;2.86  * -> mulへ汎用化
  ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z)) ; 2.86 + -> add, sqrt -> square-root
                      (square (imag-part z)))))
  (define (angle z)
    (arctangent (imag-part z) (real-part z))) ; 2.86 atan -> arctangent
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
    (cons (square-root (add (mul x x) (mul y y)))
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

(define (real-part z)(apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; install

(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)

; --- 2.84 ---
#|
  real (レベル3)
     ↓
  rational (レベル2)
     ↓
  scheme-number (レベル1)

real→rational: 整数値(1.0とか3.0)ならsrationalへ
rational→scheme-number: が1ならscheme-numberへ
scheme-number: 最下層なのでを返す

|#

(define (type-level type)
  (get 'level type))

(define (higher-type? type1 type2)
  (> (type-level type1) (type-level type2)))

; 同じになるまでやる
(define (raise-to target-type value)
  (let ((current-type (type-tag value)))
    (if (eq? current-type target-type)
        value  ; 既に同じ型なら何もしない
        (raise-to target-type (raise value)))))  ; raiseして再帰


; ---- 2.85 ---
(define (project x) (apply-generic 'project x))
(define (equ? x y) (apply-generic 'equ? x y))

(define (drop x)
  (let ((projected (project x)))
    (if (equal? (type-tag x) (type-tag projected))  ; 同じ型?
        x  ; projectできなかった
        (if (equ? x (raise projected))  ; 片方をraiseしてを比較比較
            (drop projected)
            x))))





; --- 2.5.3の本文

(define (install-polynomial-package)
  ;; 内部手続き
  ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ; ⟨2.3.2節の手続きsame-variable?とvariable?⟩
  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))


  ;; 項と項リストの表現
  ; ⟨以下の本文にある手続きadjoin-term ...coeff⟩

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))


  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

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
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  ;; システムの他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  ;; 多項式の追加演算
  (put 'level 'polynomial 5)
  (put 'project '(polynomial)      
       (lambda (p) (tag p)))
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2) #f))


  ;; 2.87
  (define (all-coeffs-zero? term-list)
    (cond ((empty-termlist? term-list) #t)
          ((=zero? (coeff (first-term term-list)))
           (all-coeffs-zero? (rest-terms term-list)))
          (else #f)))
  (put '=zero? '(polynomial)
       (lambda (p)
         (all-coeffs-zero? (term-list p))))

  ;; 2.88
  ; 符号反転
  (put 'negate '(polynomial)
       (lambda (p)
         (tag (make-poly (variable p)
                         (negate-terms (term-list p))))))

  (define (negate-terms term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (let ((first (first-term term-list)))
          (adjoin-term 
           (make-term (order first) (negate (coeff first)))
           (negate-terms (rest-terms term-list))))))

  ; sub
  (define (sub-poly p1 p2)
    (add-poly p1 (make-poly (variable p2)
                            (negate-terms (term-list p2))))) ; (p1) - (p2) -> (p1) + (-1(p2))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  ;; 2.91
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1)) ; t1 = 被除数の最高次項
              (t2 (first-term L2))) ; t2 = 除数の最高次項
          (if (> (order t2) (order t1)) ; orderが t2 > t1の場合
              (list (the-empty-termlist) L1) ; 
              (let ((new-c (div (coeff t1) (coeff t2))) ; 
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result (div-terms 
                                       (add-terms L1 
                                                  (negate-terms
                                                   (mul-term-by-all-terms 
                                                    (make-term new-o new-c) L2)))
                                       L2)
                       
                                      ))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))
                  ))))))
  ;; div-polyの実装
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1) (term-list p2))))
          (list (make-polynomial (variable p1) (car result))   ; 商
                (make-polynomial (variable p1) (cadr result)))) ; 剰余
        (error "Polys not in same var -- DIV-POLY" (list p1 p2))))

  
  ;; polynomial packageに追加
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) 
         (let ((result (div-poly p1 p2)))
           (list (tag (car result))    ; 商をタグ付け
                 (tag (cadr result)))))) ; 剰余をタグ付け


  
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (make-term order coeff) (list order coeff))


(install-polynomial-package)



; --- 2.88 ---
(display "---2.88---")
(newline)

; 汎用符号反転演算
(define (negate x) (apply-generic 'negate x))


; (-3/4)x + 3/4

; --- 2.93 ---
(display "---2.93---")
(newline)

; (define p1 (make-polynomial 'x '((2 1)(0 1))))
; (define p2 (make-polynomial 'x '((3 1)(0 1))))
; (define rf (make-rational p2 p1))
#|
修正前エラー
gcd: contract violation
  expected: rational?
  given: (polynomial x (3 1) (0 1))

> make-ratを変更し分数を最低の項まで引き下げようとしないようにせよ
make-ratでgcdの約分をしないようにした
できるようになった
|#
; --- 2.93 ---
(display "---2.93---")
(newline)


; (define p1 (make-polynomial 'x '((2 1)(0 1))))
; (define p2 (make-polynomial 'x '((3 1)(0 1))))
; ↑ でエラーになる↓みたいに中身もタグをつけたらいけた。

(define p1 (make-polynomial 'x 
                           (list (make-term 2 (make-scheme-number 1))
                                 (make-term 0 (make-scheme-number 1)))))
(define p2 (make-polynomial 'x 
                           (list (make-term 3 (make-scheme-number 1))
                                 (make-term 0 (make-scheme-number 1)))))
(define rf (make-rational p2 p1))

rf
; (rational (polynomial x (3 1) (0 1)) polynomial x (2 1) (0 1))

; > さてaddを使ってrfに自分自身を足せ. この加算手続きは分数を最低項まで引き下げないことを見よ.
; a/b + c/d = (a*d + c*b)/(b*d)
; rf + rf = (x^3+1)/(x^2+1) + (x^3+1)/(x^2+1) = [2*(x^3+1)*(x^2+1)] / [(x^2+1)^2]
; → (x^2+1) が共通なので、 2*(x^3+1) / (x^2+1)

(define rf-plus-rf (add rf rf))
#|
修正前エラー
*: contract violation
  expected: number?
  given: (polynomial x (3 1) (0 1))

汎用算術パッケージを使うようにしていく
|#

rf-plus-rf
; (rational (polynomial x (5 (scheme-number . 2)) (3 (scheme-number . 2)) (2 (scheme-number . 2)) (0 (scheme-number . 2))) polynomial x (4 (scheme-number . 1)) (2 (scheme-number . 2)) (0 (scheme-number . 1)))
; (2x^5 + 2x^3 + 2x^2 + 2) / (x^4 + 2x^2 + 1)

; 約分されていないことを確認










