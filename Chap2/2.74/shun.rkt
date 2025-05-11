#lang racket

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (make-record-a name address salary)
  (list name address salary))

(define (make-record-a salary name address)
  (list salary name address))

(define (install-a-package)
  (define (get-name r) (car r))
  (put 'get-name 'a get-name)
  'done)

(define (install-b-package)
  (define (get-name r) (cadr r))
  (put 'get-name 'b get-name)
  'done)

; a
; 全てのレコードは名前を持っていると仮定し、名前をキーとしてレコードを取得する
(define (get-record office name records)
  (if (null? records)
      '()
      (if (= ((get 'get-name office) (car records)) name)
          (attach-tag office (car records))
          (get-record office name (cdr records)))))
; get-nameという名前の手続きを持つ必要がある

; b
; 事業所のテーブルを作る
(define offices (list 'a 'b))

; 事業所のテーブル分get-recordを回す。
; そして事業所のget-salaryを呼ぶ。
; 事業所用のテーブルが必要になった。
(define (get-salary name records)
  (define (get-salary-iter name records offices)
    (if (null? offices)
      '()
      (let (office (car offices))
           (record (get-record office name records))
          (if (null? record)
              (get-salary-iter name records (cdr offices))
              (get 'get-salary office (cadr record))))))
  (get-salary-iter name records offices)
  )

; c
(define (find-employee-record name records)
  (define (find-employee-iter name records offices)
    (if (null? offices)
      '()
      (let (office (car offices))
           (record (get-record office name records))
          (if (null? record)
              (find-employee-iter name records (cdr offices))
              record)))) ; bの返す値をrecordにしただけ
  (find-employee-iter name records offices)
  )

; d
; 専用のパッケージの作成、事業所のテーブルへの追加ができれば良い