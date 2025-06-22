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

(define (apply-generic op . args)
  (define (all-same? lst)
    (if (null? lst)
        #t
        (let ((first (car lst)))
          (andmap (lambda (x) (equal? x first)) lst))))

  ;; 全ての引数について強制型変換を試みる
  (define (try-coerce-all args target-type)
    ;; １つの引数をターゲット型に強制変換する
    ;; 変換できない場合は #f を返す
    (define (coerce-arg arg arg-type)
      (if (eq? arg-type target-type)
          arg
          (let ((coercion (get-coercion arg-type target-type)))
            (if coercion
                (coercion arg)
                #f))))
    ;; map で各引数ごとに強制型変換を試みて、#f があれば失敗
    (let* ((type-tags (map type-tag args))
           (coerced-args (map coerce-arg args type-tags)))
      (if (andmap (lambda (x) x) coerced-args)
          coerced-args
          #f)))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ;; 引数が3つ以上の場合、再帰的に処理を試みる
          (if (> (length args) 2)
              (let ((first (car args))
                    (rest (cdr args)))
                (apply-generic op first (apply apply-generic op rest)))
              ;; 引数が2つの場合、型強制変換を試みる
              (if (all-same? type-tags)
                  (error "[same] No method for these types"
                         (list op type-tags))
                  (let ((types type-tags))
                    (define (try-coercions types-to-try)
                      (if (null? types-to-try)
                          (error "No method for these types"
                                 (list op type-tags))
                          (let ((coerced-args (try-coerce-all args (car types-to-try))))
                            (if coerced-args
                                (apply apply-generic op coerced-args)
                                (try-coercions (cdr types-to-try))))))
                    (try-coercions types))))))))

(define (add . args) (apply apply-generic 'add args))
(define (sub . args) (apply apply-generic 'sub args))
(define (mul . args) (apply apply-generic 'mul args))
(define (div . args) (apply apply-generic 'div args))
(define (exp x y) (apply-generic 'exp x y)) ;; 問題 2.81 によって追加

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
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

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
  (put 'real-part '(complex) 'real-part)
  (put 'imag-part '(complex) 'imag-part)
  (put 'magnitude '(complex) 'magnitude)
  (put 'angle '(complex) 'angle)
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
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


;; 使用例
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

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

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)

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

(provide add sub mul div exp
         make-scheme-number make-rational
         make-complex-from-real-imag make-complex-from-mag-ang
         scheme-number->rational scheme-number->complex)
