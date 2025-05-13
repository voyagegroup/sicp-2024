#lang sicp
; get/put（3.3.3より）
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

; 2.4.2
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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))


(define (polar? z)
  (eq? (type-tag z) 'polar))

; 2.74ここから

; 東京事業所の従業員データの一例
(define tokyo-record '(taro (address saitama) (salary 1000)))
tokyo-record
; 名前
(define (name record)
  (car record))
(name tokyo-record)

; address
(define (address record)
  ; (car (cdr (car (cdr record)))))
  (cadadr record))
(address tokyo-record)

; salary
(define (salary record)
  (car (cdr (car (cdr (cdr record))))))
(salary tokyo-record)

; get record

; tokyo packageを作る
(define (install-tokyo-package)
  (define (name record)
    (car record))
  
  (define (address record)
    (cadadr record))

  (define (salary record)
    (car (cdr (car (cdr (cdr record))))))

  (define (get-record records name)
    (let ((entry (assoc name records)))
      (if entry
          (cons name (cdr entry))
          #f)))
    

  (put 'get-name 'tokyo name)
  (put 'get-address 'tokyo address)
  (put 'get-salary 'tokyo salary)
  (put 'get-record 'tokyo get-record)
  'done)

(install-tokyo-package)

; a. get-record
(define tokyo-file
  (attach-tag 'tokyo
    '((taro (address saitama) (salary 1000))
      (hanako (address chiba) (salary 1200)))))

(define (get-record name file)
  ((get 'get-record (type-tag file)) (contents file) name))

(get-record 'taro tokyo-file)
; (taro (address saitama) (salary 1000))
(get-record 'hanako tokyo-file)
; (hanako (address chiba) (salary 1200))


; b. get-salary
(define (get-salary name file)
  (let ((record (get-record name file)))
    ((get 'get-salary (type-tag file)) record)))

(get-salary 'taro tokyo-file)
; 1000

; c. 本部のために, find-employee-record手続きを実装せよ. すべての事業所ファイルから与えられた従業員のレコードを探し, それを返すものとする. この手続きは引数として従業員の名前と全事業所ファイルのリストをとるものと仮定せよ.

; とりあえずもう一つつくる
; (name address salary) の形式
(define (install-osaka-package)
  (define (name record)
    (list-ref record 0))

  (define (address record)
    (list-ref record 1))

  (define (salary record)
    (list-ref record 2))

  (define (get-record records name)
    (let ((entry (assoc name records)))
      (if entry
          (cdr entry)
          #f)))

  (define (get-record-osaka records name)
    (let ((entry (assoc name records)))
      (if entry
          (cons name (cdr entry))
          #f)))

  (put 'get-name 'osaka name)
  (put 'get-address 'osaka address)
  (put 'get-salary 'osaka salary)
  (put 'get-record 'osaka get-record-osaka)

  'done)

(define osaka-file
  (attach-tag 'osaka
    '((jiro saitama 10000)
      (sabro chiba 12000))))
(install-osaka-package)
(get-record 'jiro osaka-file)
(get-salary 'jiro osaka-file)

(define (find-employee-record files name)
  (if (null? files)
      #f ; 見つからなかったら偽
      (let ((record (get-record name (car files))))
        (if record
            record
            (find-employee-record (cdr files) name)))))

(find-employee-record (list tokyo-file osaka-file) 'taro)
; (taro (address saitama) (salary 1000))
(find-employee-record (list tokyo-file osaka-file) 'jiro)
; (jiro saitama 10000)

; d. この企業が, 別の会社を合併した時, 新しい従業員情報を中央システムに組み込むには, どういう変更をすべきか.
; 新しいパッケージを作成する


