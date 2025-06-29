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

; --- 2.52のapply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2) ; c)でこれを追加
                    (error "同じ型で変換しようとしています") ; 追加
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
                )
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

; --- 2.5.1
(define (add x y) (apply-generic 'add x y))
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
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

; ---

; --- 2.81

coercion-list
; ()
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

coercion-list
; ((complex complex #<procedure:complex->complex>) (scheme-number scheme-number #<procedure:scheme-number->scheme-number>))

; --- 2.81 (a)
; 汎用べき乗演算を定義
(define (exp x y) (apply-generic 'exp x y))

; scheme-numberパッケージにexp演算を追加(問題文の通り)
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (attach-tag 'scheme-number (expt x y))))

(exp (make-scheme-number 2) (make-scheme-number 3))
; -> (scheme-number . 8)

; とりあえず、complexという名前で適当なパッケージを作成
(define (install-complex-package)
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex)
       (lambda (x y) (tag (+ x y))))
  (put 'make 'complex
       (lambda (x) (tag x)))
  'done)

(define (make-complex n)
  ((get 'make 'complex) n))

(install-complex-package)


; (exp (make-complex 2) (make-complex 3))
; 無限ループした
; (get 'exp '(complex complex)) を表から検索して見つからないので強制型変換を試みる
; (put-coercion 'complex 'complex complex->complex) がヒットする
; (define (complex->complex z) z) はなにもせずにそのまま帰すだけなので、ループし続ける
; ---


; --- 2.81 (b)
; 正しくない
; aのように無限ループの原因になってしまう
; ---



(exp (make-complex 2) (make-complex 3))
