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
                    (eq? op 'div))
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
       (lambda (x) (make-rational x 1)))
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
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
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
       (lambda (x) (= (numer x) 0)))

  ;; rational → real
  (put 'raise '(rational)
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'level 'rational 2); 追加
 

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  ;; 2.85
  (put 'project '(rational)
       (lambda (x)
         (if (= (denom x) 1)  ; 分母が1の場合のみ
             (make-scheme-number (numer x))
             (tag x))))  ; 変換できない場合はrational のまま
  (put 'equ? '(rational rational)
       (lambda (x y) 
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))

  ;; 2.86
  (put 'square-root '(rational)
       (lambda (x) 
         (make-real (sqrt (/ (numer x) (denom x))))))  ; 有理数→実数で計算
  (put 'arctangent '(rational rational)
       (lambda (y x) 
         (make-real (atan (/ (numer y) (denom y)) 
                          (/ (numer x) (denom x))))))
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
             (make-rational (round x) 1) ; 変換できる場合はroundする
          
             (tag x))))   ; 変換できない場合はreal のまま
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))

  ;; 2.86
  (put 'square-root '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'arctangent '(real real)
       (lambda (y x) (tag (atan y x))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag (tag x) (make-scheme-number 0))))
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



; 動作検証

; テスト用のデータを作成
(define real-5 (make-real 5.0))  ; 整数値の実数
(define real-2-5 (make-real 2.5))  ; 非整数値の実数
(define rat-5-1 (make-rational 5 1))  ; 分母が1の有理数
(define rat-5-2 (make-rational 5 2))  ; 分母が1でない有理数

; dropのテスト
(drop real-5)      ; (real . 5.0) → (scheme-number . 5) になるはず
; -> (real . 5.0)
(drop real-2-5)    ; (real . 2.5) → そのまま(下げられない)
; -> (real . 2.5)
(drop rat-5-1)     ; (rational 5 . 1) → (scheme-number . 5) になるはず
; -> (scheme-number . 5)
(drop rat-5-2)     ; (rational 5 . 2) → そのまま(下げられない)
; -> (rational 5 . 2)

; 答えをかする
(add (make-rational 3 1) (make-rational 2 1))
; 修正前
; (rational 5 . 1)
; 修正後
; (scheme-number . 5)

; --- 2.86 ---
(display "---2.86---")
(newline)

(define c1 (make-complex-from-real-imag (make-rational 3 4) (make-rational 1 2)))
c1 ;-> (complex rectangular (rational 3 . 4) rational 1 . 2)
(define c2 (make-complex-from-real-imag (make-scheme-number 1) (make-scheme-number 1)))
c2 ; -> (complex rectangular (scheme-number . 1) scheme-number . 1)

; 複素数の足し算
; (3/4 + 1/2i) + (1 + 1i)
(add c1 c2)

; real-part: contract violation
;  expected: complex-number?
;  given: (rectangular (rational 3 . 4) rational 1 . 2)
; + 演算でえらーになっていそう
; addに変えた


; -> (complex rectangular (rational 7 . 4) rational 3 . 2)
; yosasou

(sub c1 c2)
; subにかえた
; -> (complex rectangular (rational -1 . 4) rational -1 . 2)


(mul c1 c2)
; mulとaddに変えた。squareでエラーになっている
; *: contract violation
;   expected: number?
;   given: (rational 3 . 4)

; squareとmagnitudeをmulとaddへ。あーね。sqrtが対応してないのね。
; sqrt: contract violation
;   expected: number?
;   given: (rational 13 . 16)

; square-rootを定義した。
; angleのatanもよしなにしないとなのか
;   (define (angle z)
;     (atan (imag-part z) (real-part z)))

; -> (complex polar (real . 1.2747548783981963) real . 1.3734007669450157)
; yosasou

(div c1 c2)
; -> (complex polar (real . 0.6373774391990981) real . -0.19739555984988078)

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

  ;; 2.87
  (define (all-coeffs-zero? term-list)
    (cond ((empty-termlist? term-list) #t)
          ((=zero? (coeff (first-term term-list)))
           (all-coeffs-zero? (rest-terms term-list)))
          (else #f)))
  (put '=zero? '(polynomial)
       (lambda (p)
         (all-coeffs-zero? (term-list p))))

  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (make-term order coeff) (list order coeff))


(install-polynomial-package)

; --- 2.87 ---
(display "---2.87---")
(newline)


; 3x^2 + 2x + 1
(define poly
  (make-polynomial 'x 
                   (list (make-term 2 (make-scheme-number 3))
                         (make-term 1 (make-scheme-number 2))
                         (make-term 0 (make-scheme-number 1)))))

; 0x^2 + 0x + 0
(define zero-poly 
  (make-polynomial 'x 
                   (list (make-term 2 (make-scheme-number 0))
                         (make-term 1 (make-scheme-number 0))
                         (make-term 0 (make-scheme-number 0)))))

(=zero? poly)
; → #f
(=zero? zero-poly)
; → #t

(add poly zero-poly)
; (polynomial x (2 (scheme-number . 3)) (1 (scheme-number . 2)) (0 (scheme-number . 1)))


(define poly-2
  (make-polynomial 'x 
                   (list (make-term 2 (make-scheme-number 6))
                         (make-term 1 (make-scheme-number 5))
                         (make-term 0 (make-scheme-number 4)))))

(add poly-2 poly)
; (polynomial x (2 (scheme-number . 9)) (1 (scheme-number . 7)) (0 (scheme-number . 5)))