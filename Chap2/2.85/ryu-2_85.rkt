#lang sicp


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
          ; (apply proc (map contents args))　もともと
          ; (drop (apply proc (map contents args))) ループした？
          (let ((result (apply proc (map contents args))))
            (if (or (eq? op 'raise) ; drop自体がapply-genericを呼ぶ演算は除外する必要があった
                    (eq? op 'project)
                    (eq? op 'equ?)
                    (eq? op 'level))
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

  ;; int → rational
  (put 'raise '(scheme-number)
      (lambda (x) (make-rational x 1)))
  (put 'level 'scheme-number 1) ;levelを追加

  ;; 2.85
    (put 'project '(scheme-number)
     (lambda (x) (tag x)))  ; 最下層なので自分自信をそのまま帰す
  (put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))
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

  ;; 2.85で追加
  (put 'project '(real)
     (lambda (x)
       (if (integer? x) ; 5.0 -> #t, 5.1 -> #f
           (make-rational (round x) 1) ; 変換できる場合はroundする
          
           (tag x))))   ; 変換できない場合はreal のまま
  (put 'equ? '(real real)
     (lambda (x y) (= x y)))
  'done)

(define (make-real x)
  ((get 'make 'real) x))

; install

(install-scheme-number-package)
(install-rational-package)
(install-real-package)

; --- 2.84 ---
#|
  real (レベル3)
     ↓
  rational (レベル2)
     ↓
  scheme-number (レベル1)

real→rational: 整数値（1.0とか3.0）ならsrationalへ
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

















