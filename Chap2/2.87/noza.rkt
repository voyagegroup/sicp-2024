#lang racket

(require rnrs/mutable-pairs-6)

(define (square x) (* x x))

; ----- テーブルの実装 -------
(define (massoc key mlist)
  (cond
    [(null? mlist) #f]
    [(not (mpair? mlist)) (error "massoc: not a proper mutable list" mlist)]
    [(equal? key (mcar (mcar mlist))) (mcar mlist)]
    [(mpair? (mcdr mlist)) (massoc key (mcdr mlist))]
    [else #f])) ; improper end → not found

(define (lookup key-1 key-2 table)
  (let ((subtable (massoc key-1 (mcdr table))))
    (if subtable
        (let ((record (massoc key-2 (mcdr subtable))))
          (if record
              (mcdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (massoc key-1 (mcdr table))))
    (if subtable
        (let ((record (massoc key-2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (set-mcdr! subtable
                         (mcons (mcons key-2 value)
                                (mcdr subtable)))))
        (set-mcdr! table
                   (mcons
                    (mcons key-1
                           (mcons (mcons key-2 value) '()))
                    (mcdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (mcons '*table* '())))

    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons
                        (mcons key-1
                               (mcons (mcons key-2 value) '()))
                        (mcdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc!) insert!]
        [else (error "Unknown operation -- TABLE" m)]))

    dispatch))
; ; ----- テーブルの実装（おわり） -------

; タグ周りの実装
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
; タグ周りの実装（おわり）

;; テーブルの初期化
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; 1. 型の階層を定義する
(define (type-level type)
  (cond [(eq? type 'scheme-number) 0]
        [(eq? type 'rational) 1]
        [(eq? type 'complex) 2]
        [else (error "Unknown type -- TYPE-LEVEL" type)]))

;; 2. 目的の型まで型を上げる
(define (raise-to-type value target-type)
  (let ((current-type (type-tag value)))
    (cond [(eq? current-type target-type) value]
          [(< (type-level current-type) (type-level target-type))
           (raise-to-type (raise value) target-type)]
          [else (error "Cannot raise to lower type"
                       (list current-type target-type))])))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
            ;; 結果を単純化する（ただし、project, drop, raise, equ?, =zero?は除く）---------
            ;; 除かないと、数値でない型や drop された型が返ってきてしまう
            (if (memq op '(project drop raise equ? =zero?))
                result
                (drop result)))
            ;; ---------------------------------------------------------------
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "[same] No method for these types"
                           (list op type-tags))
                    (let ((level1 (type-level type1))
                          (level2 (type-level type2)))
                      (cond ((< level1 level2)
                             (apply-generic op (raise-to-type a1 type2) a2))
                            ((> level1 level2)
                             (apply-generic op a1 (raise-to-type a2 type1)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              ;; 引数が3つ以上の場合、左から順に処理
              (if (> (length args) 2)
                  (let* ((first (car args))
                         (second (cadr args))
                         (rest (cddr args))
                         (result (apply-generic op first second)))
                    (apply apply-generic op (cons result rest)))
                  (error "No method for these types"
                         (list op type-tags))))))))

(define (add . args) (apply apply-generic 'add args))
(define (sub . args) (apply apply-generic 'sub args))
(define (mul . args) (apply apply-generic 'mul args))
(define (div . args) (apply apply-generic 'div args))
(define (exp x y) (apply-generic 'exp x y)) ;; 問題 2.81 で追加
(define (raise x) (apply-generic 'raise x)) ;; 問題 2.83 で追加
(define (equ? x y) (apply-generic 'equ? x y)) ;; 問題 2.85 で追加
(define (project x) (apply-generic 'project x)) ;; 問題 2.85 で追加
(define (=zero? x) (apply-generic '=zero? x)) ;; 問題 2.87 で追加

; drop 手続きの定義
(define (drop x)
  (let ((type (type-tag x)))
    (cond [(eq? type 'scheme-number) x] ; 塔のレベルが0にしても良いかも
          [(can-drop? x)
           (drop (project x))]
          [else x])))

;; オブジェクトが下げられるかチェック
;; 数をprojectし, 結果をraiseして出発した型に戻した時, 出発したのと同じ何かで終れば, 数は切り下げられる.
;; 上記をうまく実装できなかったので、愚直に定義した
(define (can-drop? x)
  (let ((type (type-tag x)))
    (cond [(eq? type 'scheme-number) #f]  ; scheme-numberは最下層
          [(eq? type 'rational)
           (= (cdr (contents x)) 1)]  ; denom = 1
          [(eq? type 'complex)
           (= (cdr (contents (contents x))) 0)]  ; imag-part = 0
          [else #f])))

; scheme-number の定義
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
  (put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))
  (put 'raise '(scheme-number)
       (lambda (x) ((get 'make 'rational) x 1)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

; rational-package の定義
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
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))

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
  (put 'raise '(rational)
       (lambda (x) ((get 'make-from-real-imag 'complex)
                    (/ (numer x) (denom x)) 0)))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))
  (put 'project '(rational)
       (lambda (x)
         (if (= (denom x) 1)
             ((get 'make 'scheme-number) (numer x))
             ((get 'make 'scheme-number) (/ (numer x) (denom x))))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

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
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (real-part z)
    (let ((proc (get 'real-part (list (type-tag z)))))
      (proc (contents z))))
  (define (imag-part z)
    (let ((proc (get 'imag-part (list (type-tag z)))))
      (proc (contents z))))
  (define (magnitude z)
    (let ((proc (get 'magnitude (list (type-tag z)))))
      (proc (contents z))))
  (define (angle z)
    (let ((proc (get 'angle (list (type-tag z)))))
      (proc (contents z))))

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
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ-complex? z1 z2)))
  (put 'project '(complex)
       (lambda (z)
         (let ((real (real-part z)))
           (cond [(integer? real)
                  ((get 'make 'rational) real 1)]
                 [else
                  ((get 'make 'rational)
                   (numerator real) (denominator real))]))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 型強制変換のリスト（2.85から）
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

;; パッケージのインストール
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; 簡単のために、scheme-number, rational, complex の生成手続きを定義
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; 型強制変換の定義と登録
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(define (scheme-number->rational n)
  (make-rational (contents n) 1))
(put-coercion 'scheme-number 'rational scheme-number->rational)
(put-coercion 'scheme-number 'complex scheme-number->complex)

;; ===========================================
;; 多項式パッケージの実装
;; ===========================================

(define (install-polynomial-package)
  ;; 内部手続き
  ;; 多項式の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

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

  ; 多項式がゼロかどうかを判定する内部手続き
  (define (zero-polynomial? p)
    (define (zero-term-list? terms)
      (or (null? terms)
          (and (zero-coeff? (cadr (car terms)))
              (zero-term-list? (cdr terms)))))
    (define (zero-coeff? coeff)
      (cond [(number? coeff) (= coeff 0)]
            [(pair? coeff) (=zero? coeff)]
            [else #f]))
    (zero-term-list? (cdr p)))  ; term-list

  ;; 多項式の加算
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  ;; 多項式の乗算
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;; 項リストの加算
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

  ;; 項リストの乗算
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
  (put '=zero? '(polynomial)
       (lambda (p) (zero-polynomial? p)))
  'done)

;; polynomial パッケージのインストール
(install-polynomial-package)

;; 多項式のコンストラクタ
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; ===========================================
;; =zero? のテスト
;; ===========================================

;; テスト用のデータ定義
(define zero-poly (make-polynomial 'x '()))  ; 空の多項式 = ゼロ多項式
(define zero-poly2 (make-polynomial 'x (list (list 2 0) (list 1 0) (list 0 0))))  ; 係数がすべて0の多項式
(define non-zero-poly (make-polynomial 'x (list (list 2 1) (list 1 0) (list 0 3))))  ; 1*x^2 + 0*x + 3 = x^2 + 3
(define mixed-zero-poly (make-polynomial 'x (list (list 3 0) (list 1 2))))  ; 0*x^3 + 2*x

;; テスト実行
(define (test-=zero?)
  (display "=zero? テスト開始\n")
  (display "空の多項式: ")
  (display (=zero? zero-poly))
  (display "\n")
  (display "係数がすべて0の多項式: ")
  (display (=zero? zero-poly2))
  (display "\n")
  (display "非零多項式: ")
  (display (=zero? non-zero-poly))
  (display "\n")
  (display "一部係数が0の多項式: ")
  (display (=zero? mixed-zero-poly))
  (display "\n")
  (display "=zero? テスト完了\n"))

(test-=zero?)
